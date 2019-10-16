% Written by Mohamed Elshamy (mohamed.elshamy@usask.ca) in 2017-2018

% This script is written to extract data from CanRCM4 (netcdf) over a given basin
% and writes it to seq file(s) as read by MESH
% It processes one variable and segments the output to different time periods
% as needed with a one year overlap period
% (e.g. if segment 1 goes from 1950-1970, the next one will go from 1970-
% with 1970 included in both segments. It also calculates basin average
% time series for each time step and then summarizes these as monthly
% series at the end (ignoring Feb 29)
% Please refer to the README file that comes with the dataset to know how
% it is structured. THis script does not create the output folder
% structure. It has to be created by the user based on the time segments
% defined by Y1 and Y2
% Please study the whole script before using it - modify as needed. Some
% commented line may be activitated for testing but have to be commented
% before running for long as they will consume memory and make your system
% unstable


tic;    clc;    clear;  
% first select the variable, scenario idetifier and group - 
% These are used to construct the file name and
% read the variable from the nc file and the output files
x = 'ps';
sc = 'r10i2p1';
group = 'r4';

% The Y1 and Y2 vectors indicate the start and end indices for the
% different segments of the output data files. 6 segments are defined
Y1 = [1 30 56 64 92 122]; Y2 = [30 56 64 92 122 151];
%=====================================================================================================
% open MESH_drainage_database file - necessary to know the extents to be
% read and the RANK to sort the data for seq output
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\07_MRB_Drainage_Database_MEAME_Real_Lakes_12GRUsEW_Split_2005_v2.r2c','r');
fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\08_MRB_Drainage_Database_MEAME_Real_Lakes_15GRUs_ENWSW_Split_2005.r2c','r');

% Read the Header & Rank values
 
% read drainage database header 
tline = fgetl(fid_dr);
while strcmp(tline(1:10),':EndHeader')== 0 
%     fprintf(fid_w,'%s\n',tline);     
     tline = fgetl(fid_dr);
     if length(tline) < 20; tline = [tline blanks(20-length(tline))]; end
     if strcmp(tline(1:16),':TotalNumOfGrids')==1; num_grids=str2num(tline(17:end)); end  % num_grids is NO. of grids in the shd file
%     if strcmp(tline(1:11),':ClassCount')== 1; num_GRU=str2num(tline(12:end)); end % num_GRU is NO. of GRUs in the shd file
     if strcmp(tline(1:7),':xCount')==1; xCount=str2num(tline(8:end)); end  % num_Col is NO. of rows in the shd file
     if strcmp(tline(1:7),':yCount')==1; yCount=str2num(tline(8:end)); end  % num_Row is NO. of columns in the shd file
     if strcmp(tline(1:8),':xOrigin')==1; xOrigin=str2num(tline(9:end)); end  % num_Col is NO. of rows in the shd file
     if strcmp(tline(1:8),':yOrigin')==1; yOrigin=str2num(tline(9:end)); end  % num_Row is NO. of columns in the shd file
     if strcmp(tline(1:7),':xDelta')==1; xDelta=str2num(tline(8:end)); end  % num_Col is NO. of rows in the shd file
     if strcmp(tline(1:7),':yDelta')==1; yDelta=str2num(tline(8:end)); end  % num_Row is NO. of columns in the shd file
end

xOrigin = xOrigin + xDelta/2;
yOrigin = yOrigin + yDelta/2;

xmodel = xOrigin:xDelta:(xOrigin+xDelta*xCount);
ymodel = yOrigin:yDelta:(yOrigin+yDelta*yCount);

xmodel = xmodel(1:end-1) + 360;
ymodel = ymodel(1:end-1);
[xmodel,ymodel]=meshgrid(xmodel,ymodel);

% Now read the rank matrix from drainage database - it needs to be
% transposed because fscanf reads columns first
Rank = fscanf(fid_dr,'%f',[xCount yCount]);
Rank = transpose(Rank);
% Convert the Rank matrix and data matrix into vectors, then sort by Rank then remove all zeros
Rank1 = reshape(Rank,xCount*yCount,1);

% Sort in descending order so that the first element becomes the Number of
% Active Elements
SortedRanks = sort(Rank1,'descend');
ActiveElements = SortedRanks(1);
 
if num_grids ~=ActiveElements
     fprintf('%s\n', 'drainage database grid size mismatch');
     fclose(fid_dr);
%     fclose(fid_data);
     exit
end
fclose(fid_dr);
%===========================================================================================
% This part sets up folders and file names to read from CanRCM4 output
% it depends a lot on how the data is structured and placed
% CanRCM4 output is organized in annual files with each variable in a
% seperate folder

addpath(genpath('C:\Program Files\MATLAB\Tools\'));
forcing_file_directory = (['V:\Model_Output\110_CanRCM4-44\' x '\' sc group '\'])
%output folder - has to be created by the user before starting including a
%subfolder for the variable processed
Workspace_Path =(['V:\Model_Output\110_CanRCM4_MRB1\' sc group '\' x '\']) 

addpath(forcing_file_directory);
%load ('D:\MRB_Climate_Forcing\01_Princeton\R2C_Reprocessed\MRB_Mask.mat');

dir_data=dir(forcing_file_directory);    dir_index=[dir_data.isdir]';    file_list_all={dir_data(~dir_index).name}';

str=[x '_NAM-44_CCCma-CanESM2_historical-' group '_' sc '_CCCma-CanRCM4_r2_1hr_'];       comparison=strncmp(str,file_list_all,length(str));

addpath(forcing_file_directory);

file_list=file_list_all(comparison);

% Save the year no. from file list. The no. of seconds in new year file start from zero. 
years_file_list=regexprep(file_list,str,'');
years_file_list=regexprep(years_file_list,'.nc','');
years_file_list= cellfun(@(S) {S(1:end-17)}, years_file_list, 'UniformOutput', false);
years_file_list=cellfun(@str2double, years_file_list);
%File_Info=ncinfo(sprintf('%s',file_list{1}));
formatout=('yyyy/mm/dd HH:MM:SS.000');

for p=3:3 %which segments to process
    % open seq file for writing
    fn = [Workspace_Path str num2str(years_file_list(Y1(p))) '_' num2str(years_file_list(Y2(p))) '.seq']
    fid_seq = fopen(fn,'w');
    Data(yCount,xCount,24)=zeros;

    for yr=Y1(p):Y2(p) %length(file_list)   %   yearly file counter. iterate through each yearly file 

        %all_dates_vec=[];    

        file_list(yr)
        time_var=ncread(file_list{yr},'time');
        % read lat and lon but only once
        if yr == Y1(p)
            lat = ncread(file_list{yr}, 'lat');
            lon = ncread(file_list{yr}, 'lon');
        end
        %time_bnds=ncread(sprintf('%s',file_list{yr,:}),'time_bnds');

        n=1; f=1;        % Frame Counters, f discards Feb 29
        for i=1:365      %   daily time counter      
            Raw_Data=ncread(file_list{yr}, x ,[1 1 f], [155 130 24]);       %reads 24 frames, i.e. one day at a time
            %activate for debugging - comment for processing
            %otherwise it eats the memory and slows down the system
            %figure
            %figure1=imagesc(flipud(Raw_Data(:,:,1)'));
            for j = 1:24
                elapsed_time = datenum(years_file_list(yr),1,1,0,0,0)+n/24;     
                date_str=datestr(elapsed_time,formatout);   %   covert date to string format. the character length will be uniform in this format
                fprintf('%s\n',date_str);

                Year = years_file_list(yr);
                Month = str2num(date_str(6:7));
                Day = str2num(date_str(9:10));
                Hour = str2num(date_str(12:13));
                Minute = str2num(date_str(15:16));
                Second = str2num(date_str(18:19));
                Millisecond =str2num(date_str(21:23));

                % This is the interpolation step, xmodel and ymodel are
                % setup above based on data from the drainage database
                Data(:,:,j) = griddata(lon,lat,Raw_Data(:,:,j),xmodel,ymodel,'linear');
                %MRB(:,:,j) = MRB(:,:,j).* MRB_Mask;
                Data1 = reshape(Data(:,:,j),xCount*yCount,1);
                % Put Rank and Data into a 2 column matrix
                X = [Rank1 Data1];
                % Sort in Ascending order by Rank
                B = sortrows(X,1);
                % Extract the active elements from the data
                SortedData(:,j) = B(size(B,1)-ActiveElements+1:size(B,1),2);

                %activate for debugging - comment for processing
                %otherwise it eats the memory and slows down the system
                %figure
                %figure5=imagesc(MRB(:,:,j));        
                seqHeader = [n n Year Month Day Hour Minute Second Millisecond]; 
                recordlength = 4*length(seqHeader);
                fwrite(fid_seq,recordlength,'int');     %this is required to be read in FORTRAN as unformatted binary file
                fwrite(fid_seq,seqHeader,'int');
                fwrite(fid_seq,recordlength,'int');

                recordlength = 4 * ActiveElements;
                fwrite(fid_seq,recordlength,'int');
                fwrite(fid_seq,SortedData(:,j),'real*4');
                fwrite(fid_seq,recordlength,'int');
                var(f,yr) = mean(SortedData(:,j));  % This is used to summarize the data over the basin
                n = n + 1;
                f = f + 1;
            end

            % CanRCM4 uses a 365-day calendar so we need to add data for
            % Feb 29 on leap years - f is not incremented because Feb 29 is
            % ignored in the basin summary
            if mod(years_file_list(yr),4) == 0 %Leap year
                if (n-1)/24 == 59 %The coming day is Feb 29th on a leap year which is not on file
                    % Repeat all frames of day 59 (Feb 28)
                    for j = 1:24
                        elapsed_time = datenum(years_file_list(yr),1,1,0,0,0)+n/24;     
                        date_str=datestr(elapsed_time,formatout);   %   covert date to string format. the character length will be uniform in this format
                        fprintf('%s\n',date_str);

                        Year = years_file_list(yr);
                        Month = str2num(date_str(6:7));
                        Day = str2num(date_str(9:10));
                        Hour = str2num(date_str(12:13));
                        Minute = str2num(date_str(15:16));
                        Second = str2num(date_str(18:19));
                        Millisecond =str2num(date_str(21:23));

                        seqHeader = [n n Year Month Day Hour Minute Second Millisecond]; 
                        recordlength = 4*length(seqHeader);
                        fwrite(fid_seq,recordlength,'int');     %this is required to be read in FORTRAN as unformatted binary file
                        fwrite(fid_seq,seqHeader,'int');
                        fwrite(fid_seq,recordlength,'int');

                        recordlength = 4 * ActiveElements;
                        fwrite(fid_seq,recordlength,'int');
                        fwrite(fid_seq,SortedData(:,j),'real*4');
                        fwrite(fid_seq,recordlength,'int');

                        n = n + 1;
                    end
                end
            end            
        end
        toc/60/60
        beep   
    end
    fclose(fid_seq);
end

% This part further summarizes the time step spatial averages to a monthly
% time series - Feb 29 is ignored
d = [31 28 31 30 31 30 31 31 30 31 30 31]
s = 1;e = 1;
for m = 1:12
    e = e + d(m) * 24;
    v(m,:) = mean(var(s:e-1,:));
    s = e;
end
csvwrite([Workspace_Path str x '-' sc group '-' num2str(p) '.csv'],var) %time step summary
csvwrite([Workspace_Path str x '-' sc group '-' num2str(p) '-summary.csv'],v) %monthly summary
fprintf('%s\n', ' ************** SUCCESSFUL FINISH ***************');

toc/60/60
beep
