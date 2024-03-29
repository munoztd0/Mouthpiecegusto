function GLM_stLevel(subID)

% compute first level model contrasts reward-control
% no PM -> covariate 2nd level

% dbstop if error

%% define experiment setting parameters
subj       =   subID; % to run on server

%% What to do
firstLevel    = 0;
constrasts    = 1;
copycontrasts = 1;


%% define task variable
sessionX = 'second';
task = 'hedonicreactivity';
name_ana = 'GLM_GUSTO'; % output folder for this analysis

%% DEFINE PATH
cd ~
home = pwd;
homedir = [home '/OBIWAN'];

control = [homedir '/DERIVATIVES/GLM/SPM/' task '/' name_ana '/sub-control*'];
obese = [homedir '/DERIVATIVES/GLM/SPM/' task '/' name_ana '/sub-obese*'];

controlX = dir(control);
obeseX = dir(obese);

subj = vertcat(controlX, obeseX);

funcdir  = fullfile(homedir, '/DERIVATIVES/PREPROC');% directory with  post processed functional scans
mdldir   = fullfile (homedir, '/DERIVATIVES/GLM/SPM', task);% mdl directory (timing and outputs of the analysis)
groupdir = fullfile (mdldir,name_ana, 'group/');

addpath('/usr/local/external_toolboxes/spm12/');

%% specify fMRI parameters
param.TR = 2;
param.im_format = 'nii'; 
param.ons_unit = 'secs'; 
spm('Defaults','fMRI');
spm_jobman('initcfg');



%% define experimental design parameters
param.task = {task};
param.Cnam     = cell (length(param.task), 1);
param.duration = cell (length(param.task), 1);


%subj          = {'100'    ;'102'    ;'105'    ;'106'    ;'107'    ;'108'
%;'109'    ;'110'    ;'112'    ;'113'    ;'114'    ;'115'    ;'116'
%;'118'    ;'119'    ;'120'    ;'121'    ;'122'    ;'125'    ;'126'
%;'127'    ;'128'    ;'129'    ;'130'    ;'131'    ;'132'    ;'133'    };
% to run locally

for i = 1:length(param.task)
    
    % Specify each conditions of your desing matrix separately for each session. The sessions
    % represent a line in Cnam, and the conditions correspond to a item in the line
    % these names must correspond identically to the names from your ONS*mat.
    param.Cnam{i} = {'ONS.onsets.trialstart',... %1
        'ONS.onsets.taste.reward',...%2
        'ONS.onsets.taste.control',...%3
        'ONS.onsets.liking',...%4
        'ONS.onsets.intensity',...%5
        'ONS.onsets.familiarity',...%6
        'ONS.onsets.rinse'};%7
    
    % duration of the blocks (if events, put '0'). Specify it for each condition of each session
    % the values must be included in your onsets in seconds
    param.duration{i} = {'ONS.durations.trialstart',...
        'ONS.durations.taste.reward',...
        'ONS.durations.taste.control',...
        'ONS.durations.liking',...
        'ONS.durations.intensity',...
        'ONS.durations.familiarity',...
        'ONS.durations.rinse'};
    
    % parametric modulation of your events or blocks (ex: linear time, or emotional value, or pupillary size, ...)
    % If you have a parametric modulation
    param.modulName{i} = {'none',...%1
        'none',...%2
        'none',...%3
        'none',...%4
        'none',...%5
        'none',...
        'none'};
    
    param.modul{i} = {'none',...%1
        'none',... %2
        'none',... %3
        'none',... %4
        'none',... %5
        'none',...
        'none'};
    
    % value of the modulators, If you have a parametric modulation
    param.time{i} = {'0',... %1
        '0',... %2
        '0',... %3
        '0',... %4
        '0',... %5
        '0',...
        '0'};
      
end

%% apply design for first level analysis for each participant

for i = 1:length(subj)

    subjT = subj(i).name;
    
    % participant's specifics
    subjX = char(subjT);
    subjoutdir =fullfile(mdldir,name_ana, subjT); % subj{i,1}
    subjanatdir=fullfile(funcdir, subjT, 'ses-first/anat/');
    subjfuncdir=fullfile(funcdir, subjT, ['ses-' sessionX]); % subj{i,1}
    fprintf('participant number: %s \n', subjT);
    cd (subjoutdir)

    if ~exist('output','dir');
        mkdir ('output');
    end

    %%%%%%%%%%%%%%%%%%%%% DO FIRST LEVEL ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%
    if firstLevel == 1
        [SPM] = doFirstLevel(subjoutdir,subjfuncdir,subjanatdir,name_ana,param,subjX);
    else
        cd (fullfile(subjoutdir,'output'));
        load SPM
    end

    %%%%%%%%%%%%%%%%%%%%%%%  DO CONSTRASTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if constrasts == 1
        doContrasts(subjoutdir,param, SPM);
    end


    %%%%%%%%%%%%%%%%%%%%% COPY CONSTRASTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     if copycontrasts == 1

        mkdir (groupdir); % make the group directory where contrasts will be copied
        cd (fullfile(subjoutdir,'output'))
        
        list_dir = dir(fullfile(subjoutdir,'output', 'con*'));
        list_files = '';
        for ii = 1:length(list_dir)
            copyfile(list_dir(ii).name, [groupdir, subjX '_' list_dir(ii).name])
        end
          
        display('contrasts copied!');
        
    end

end

%% function section

    function [SPM] = doFirstLevel(subjoutdir,subjfuncdir,subjanatdir, name_ana, param, subjX)

        % variable initialization
        ntask = size(param.task,1);
        nscans = [];
        scanID = [];
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %-----------------------------
        % select post processed images for each Session
        for ses = 1:ntask

            taskX = char(param.task(ses));
            smoothfolder       = [subjfuncdir '/func'];
            targetscan         =  dir (fullfile(smoothfolder, ['sub*' taskX '*smoothBold*' param.im_format]));

            tmp{ses}           = spm_select('List',smoothfolder,targetscan.name);

            % get the number of EPI for each session
            cd (smoothfolder);
            V         = dir(fullfile(smoothfolder, targetscan.name));
            [p,n,e]   = spm_fileparts(V(1).name);
            Vn        = spm_vol(fullfile(p,[n e]));
            nscans    = [nscans numel(Vn)];

            for j = 1:nscans(ses)
                scanID    = [scanID; {[smoothfolder,'/', V.name, ',', num2str(j)]}];
            end

        end

        SPM.xY.P    = char(scanID);
        SPM.nscan   = nscans;


        %-----------------------------
        % building matrix
        for ses = 1:ntask

            taskX = char(param.task(ses));

            ONSname = spm_select('List',[subjoutdir '/timing/'],[name_ana '_task-' taskX '_onsets.mat']);
            cd([subjoutdir '/timing/']) % path
            ONS = load(ONSname);
            cd([subjoutdir '/output/'])

            nconds=length(param.Cnam{ses});


            c = 0; % we need a counter because we include only condition that are non empty

            for cc=1:nconds

                if ~ isempty(eval(param.Cnam{ses}{cc})) % only if the onsets are not all 0

                    c = c+1; % update counter

                    SPM.Sess(ses).U(c).name      = {param.Cnam{ses}{cc}};
                    SPM.Sess(ses).U(c).ons       = eval(param.Cnam{ses}{cc});
                    SPM.Sess(ses).U(c).dur       = eval(param.duration{ses}{cc});
                    SPM.Sess(ses).U(c).orth      = 1; % orthogonalization on

                    SPM.Sess(ses).U(c).P(1).name = 'none';

                    if isfield (param, 'modul') % this parameters are specified only if modulators are defined in the design

                        if ~ strcmp (param.modul{ses}{cc}, 'none')

                            if isstruct (eval(param.modul{ses}{cc}))

                                mod_names = fieldnames (eval(param.modul{ses}{cc}));
                                nc = 0; % intialize the modulators count

                                for nmod = 1:length(mod_names)

                                    nc = nc+1;
                                    mod_name = char(mod_names(nmod));

                                    SPM.Sess(ses).U(c).P(nc).name  = mod_name;
                                    SPM.Sess(ses).U(c).P(nc).P     = eval([param.modul{ses}{cc} '.' mod_name]);
                                    SPM.Sess(ses).U(c).P(nc).h     = 1;
                                    
                                end


                            else
                                SPM.Sess(ses).U(c).P(1).name  = char(param.modulName{ses}{cc});
                                SPM.Sess(ses).U(c).P(1).P     = eval(param.modul{ses}{cc});
                                SPM.Sess(ses).U(c).P(1).h     = 1;
                                
                            end
                        end
                    end
                end
            end
        end

        %-----------------------------
        %multiple regressors for mvts parameters (no movement regressor
        %after ICA)
        
        %rnam = {'X','Y','Z','x','y','z'};
        for ses=1:ntask

            SPM.Sess(ses).C.C = [];
            SPM.Sess(ses).C.name = {};

        end


        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % basis functions and timing parameters

        % OPTIONS: TR in seconds
        %--------------------------------------------------------------------------
        SPM.xY.RT          = param.TR;

        % OPTIONS: % 'hrf (with time derivative)'
        %--------------------------------------------------------------------------
        SPM.xBF.name       = 'hrf';

        % OPTIONS: % 2 = hrf (with time derivative)
        %--------------------------------------------------------------------------
        SPM.xBF.order      = 1;

        % OPTIONS: % length in seconds
        %--------------------------------------------------------------------------
        SPM.xBF.length     = 0;

        %--------------------------------------------------------------------------
        SPM.xBF.UNITS      = param.ons_unit;

        % % OPTIONS: 1|2 = order of convolution
        %--------------------------------------------------------------------------
        SPM.xBF.Volterra   = 1;

        % global normalization: OPTIONS:'Scaling'|'None'
        %--------------------------------------------------------------------------
        SPM.xGX.iGXcalc    = 'None';

        % low frequency confound: high-pass cutoff (secs) [Inf = no filtering]
        %--------------------------------------------------------------------------
        SPM.xX.K(1).HParam = 128;

        % intrinsic autocorrelations: OPTIONS: 'none'|'AR(1) + w'
        %--------------------------------------------------------------------------
        SPM.xVi.form       = 'AR(1)'; 

        % specify SPM working dir for this sub
        %==========================================================================
        SPM.swd = pwd;

        % set threshold of mask
        %==========================================================================
        SPM.xM.gMT = -Inf;% set -inf if we want to use explicit masking 0.8 is the spm default

        % Configure design matrix
        %==========================================================================
        SPM = spm_fmri_spm_ui(SPM);

        % *After* configuration but before *estimation* we need to specify the explicit mask
        %--------------------------------------------------------------------------
        SPM.xM.VM  = spm_vol(char({[subjanatdir '/' subjX '_ses-first_acq-ANTsNorm_T2w.nii']})); % here enter the mask based on the subject anatomical

        % Estimate parameters
        %==========================================================================
        disp ('estimating model')
        SPM = spm_spm(SPM);

        disp ('first level done');
    end


    function [] = doContrasts(subjoutdir, param, SPM)

        % define the SPM.mat that contains the design of the first level analysis
        %------------------------------------------------------------------
        path_ana = fullfile(subjoutdir, 'output'); % path for the first level analysis
        [files]=spm_select('List',path_ana,'SPM.mat');
        jobs{1}.stats{1}.con.spmmat = {fullfile(path_ana,files)};

        % define  T constrasts in a human friendly readable way
        %------------------------------------------------------------------

        % | GET THE NAMES FROM THE ONSETS PARAMETERS OF THE SPM MODEL
        ncondition = size(SPM.xX.name,2);

        for j = 1:ncondition

            taskN = SPM.xX.name{j} (4);
            task_name  = ['task' taskN '.'];
            conditionName{j} = strcat(task_name,SPM.xX.name{j} (18:end-6));

        end

        Ct = []; Ctnames = []; ntask = size(param.task,1);
     
        % | CONSTRASTS FOR T-TESTS

        Ctnames{1} = 'reward_control';
        weightPos  = ismember(conditionName, {'task1.taste.reward'}) * 1;
        weightNeg  = ismember(conditionName, {'task1.taste.control'}) * -1;
        Ct(1,:)    = weightPos + weightNeg;
        
        Ctnames{2} = 'reward';
        weightPos  = ismember(conditionName, {'task1.taste.reward'}) * 1;
        Ct(2,:)    = weightPos;
        
        Ctnames{3} = 'control';
        weightPos  = ismember(conditionName, {'task1.taste.control'}) * 1;
        Ct(3,:)    = weightPos;
        
       
        % define F constrasts
        %------------------------------------------------------------------
        Cf = []; Cfnames = [];
        
  
        % put the contrast matrix
        %------------------------------------------------------------------
        
        % t contrasts
        for icon = 1:size(Ct,1)
            jobs{1}.stats{1}.con.consess{icon}.tcon.name = Ctnames{icon};
            jobs{1}.stats{1}.con.consess{icon}.tcon.convec = Ct(icon,:);
        end
        
        
        % task the job
        spm_jobman('run',jobs)
        
        disp ('contrasts created!')
    end



end