% Written by Mohamed Elshamy - Jan 2017 - Last Modified Dec 2018
% This script reads netCDF data and writes seq files for a given basin
% Adapted to read CanRCM4-WFDEI-GEM data
% Refer to the README in the data folder to know how the dataset is organized
% The scripts segments the data as defined by the P vector (line 78) 
% one year overlap is included, e.g. if segment 1 goes from 1950-1970,  
% the next one will go from 1970- with 1970 included in both segments.
% Time segmentation mimimizes the skipping required which can be
% substantial for large basins
% Grid is perfectly alighned for MRB, thus no interpolation needed
% inputs include MESH Drainage Database - line 34
% Path and naming pattern of ncfiles is to be supplied on line 87 
% Path and naming pattern of seq files (to be created) is to be supplied on
% line 110 and line 213. Folders have to be pre-created by the user including
% folders for segments as defined by the P vector formatted as (yyy1-yyy2) 
% A sub-basin prefix is added and need to be adjusted for your basin
% Some blocks may need to be de/activated based on the dataset calender

tic;    clc;    clear;  

%variable names, original files and then output files, file and variable
%names in the CanRCM4-WFDEI-GEM-CaPA reflect surface variables but are in
%fact at lowest model level
VAR = {'huss','pr','ps','rlds','rsds','sfcWind','tas'};  
VAR1 = {'huss','pr','ps','rlds','rsds','sfcWind','tas'};
basin = 'MRB'; % basin ID to be inserted in the output file names

% processing one variable at a time - this is the variable index in VAR 
% used to construct file names
x = 4
sc = 'r10i2p1r5'
% ==========================================================================================================
% open the current MESH_drainage_database file - all what is needed is RANK
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\07_MRB_Drainage_Database_MEAME_Real_Lakes_12GRUsEW_Split_2005_v2.r2c','r');
fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\08_MRB_Drainage_Database_MEAME_Real_Lakes_15GRUs_ENWSW_Split_2005_v2.r2c','r');
%fid_dr = fopen('D:\70_SRB\SRB_MESH_drainage_database_default.r2c','r');
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
%====== Done with the drainage database file =============================      

% P  is lists the start of time periods - the last value is the end of the
% last time period.You may change the segmentation as needed but the first
% and last years depend on the dataset
P = [1951,1979,2010,2020,2025,2055,2065,2070,2100];

T = 3; NT = 24/T;       % T is is length of the time step and NT is the number of time steps per day
SortedData = zeros(ActiveElements,NT);
    
recordlength1 = 4 * 9;
recordlength2 = 4 * ActiveElements;
formatout=('yyyy/mm/dd HH:MM:SS.000');

fn = char(strcat('V:\Model_Output\280_CanRCM4_Cor_WFDEI-GEM-CaPA\',sc,'\',VAR(x),'_', sc, '_final.nc4'))
str1 = char(strcat('E:\280_CanRCM4_Cor_WFDEI-GEM-CaPA\', sc, '\'));
str2 = char(strcat('\', VAR1(x), '_', basin, '_CanRCM4-', sc, '_WFDEI-GEM-CaPA_cor_'));
lat = ncread(fn, 'lat');        
lon = ncread(fn, 'lon');    
time = ncread(fn,'time');

% These need to be checked in case the model grid is not aligned with
% that of the nc data - it will be like nearst neighbour interpolation if
% the differences are small
x1 = int32((xOrigin - lon(1))/xDelta);
y1 = int32((yOrigin - lat(1))/yDelta);

p1 = 1;          % # of first segment to process (as in the P vector), control the last segment from the loop
base = P(p1)-1   % base for the var summary vector  
n = 1;           % Frame Counter - same like f if the data contains Feb29 and start from the beginning
f = 1+(P(p1) - P(1))*365*NT     %index of first frame depending on the time segment
Feb29 = 0;          % Feb29 counter - not needed if the data contains Feb29
for p = p1: size(P,2)-1    
    
    % The following constuct the file names based on the selected variable
    % and period - and the file naming pattern known beforehand
    % They can be replaced in case a single file/period is used and pulled 
    % before the loop - Loop counters may need to be adjusted
    sub = char(strcat(num2str(P(p)), '-', num2str(P(p+1))));    
    fno= strcat(str1,sub,str2,sub,'.seq')
    fio_seq = fopen(fno,'w');
    %This condition handles the overlap year
    if p > p1
        %f = 1+(P(p) - P(1))*365*NT
        f = f - 365*NT;     %disable if no overlap is needed 
        n = 1;Feb29 = 0;
    end
    t1 = (P(p+1) - P(p)+1)*365*NT
    for i = 1 : NT : t1

        Data(:,:,:) = ncread(fn, char(VAR(x)),[x1 y1 f], [xCount yCount NT]);  %CanRCM 3D data
        %Data(:,:,:) = ncread(fn, char(VAR(x)),[x1 y1 1 f], [xCount yCount 1 NT]);  %GEM 4D data
        if mod(f-1,365*NT)==0 
            g = 1;
        end
    % activate plotting to make sure the data is read correctly, deactivate
    % for processing otherwise it eats memory and can make the system
    % unstable
    %    figure; imagesc(flipud(Data(:,:,1)')
        for j = 1: NT
            % construct the date-time stamp - avoids descrepancies in the
            % nc files (like 22:59)
            % This needs testing whether Feb29 need be included or not
            % depedning on the time axis of the data 
            elapsed_time = (time(i)-time(1) + (j-1) * T)/24 + Feb29 + datenum(P(p),1,1,0,0,0);    
            [Year, Month, Day, Hour, Minute, Second]=datevec(elapsed_time);   
            date_str=datestr(elapsed_time,formatout);   %   covert date to string format. the character length will be uniform in this format
            %fprintf('%s\n',date_str);

            seqHeader = [n n Year Month Day Hour Minute Second 0]; 

            fwrite(fio_seq,recordlength1,'int');     %this is required to be read in FORTRAN as unformatted binary file
            fwrite(fio_seq,seqHeader,'int');
            fwrite(fio_seq,recordlength1,'int');

            Data1 = reshape(Data(:,:,j)',xCount*yCount,1);
            % Put Rank and Data into a 2 column matrix
            X = [Rank1 Data1];
            % Sort in Ascending order by Rank
            B = sortrows(X,1);
            % Extract the active elements from the data
            SortedData(:,j) = B(size(B,1)-ActiveElements+1:size(B,1),2);

            fwrite(fio_seq,recordlength2,'int');
            fwrite(fio_seq,SortedData(:,j),'real*4');
            fwrite(fio_seq,recordlength2,'int');

            % Activate var to calculate the spatial mean over the basin -
            % it is seperated by year to facilitate temporal averaging at
            % the end
            
            var(g,Year-base) = mean(SortedData(:,j));  
            fprintf('%i %i-%i-%i:%i\n',seqHeader(1),seqHeader(3),seqHeader(4),seqHeader(5),seqHeader(6));
            %var(f,int32(Year-1950)) = mean(SortedData(:,j));
            n = n + 1;
            f = f + 1;
            g = g + 1;
              
        end
        
% The following block is required if Feb29 is not in the dataset - it does not apply to GEM
         Leap = 0;
         if Month == 2 & Day == 28 %29 Feb may be coming, check if it is a leap year            
             if mod(Year,4) == 0 
                 if mod(Year,100) == 0 
                     if mod(Year,400) == 0
                         Leap = 1;
                     end
                 else
                     Leap = 1;
                 end
             end
         end           
         if Leap == 1  %Leap year is coming which is not in file, repeat data for 28 Feb             
            Feb29 = Feb29 + 1;
            for j = 1:NT
               seqHeader = [n n Year Month Day+1 (j-1)*T Minute Second 0]; 
               
               %for x=1:size(VAR,2)               
               fwrite(fio_seq,recordlength1,'int');     %this is required to be read in FORTRAN as unformatted binary file
               fwrite(fio_seq,seqHeader,'int');
               fwrite(fio_seq,recordlength1,'int');
               fprintf('%i %i-%i-%i:%i\n',seqHeader(1),seqHeader(3),seqHeader(4),seqHeader(5),seqHeader(6));

               fwrite(fio_seq,recordlength2,'int');
               fwrite(fio_seq,SortedData(:,j),'real*4');
               fwrite(fio_seq,recordlength2,'int');                               

               n = n + 1; 
             end
        end
    end
    fclose(fio_seq);
end
close('all');

% This block further summarizes the spatial means temporally to the monthly
% time step - Feb 29 is ignored. one file is produced for all processed
% segments
 
sub = [num2str(P(p1)) '-' num2str(P(p+1))];
str = [str1 str2 sub];
d = [31 28 31 30 31 30 31 31 30 31 30 31];
 s = 1;e = 1;
 for m = 1:12
     e = e + d(m) * NT;
     v(m,:) = mean(var(s:e-1,:));
     s = e;
 end
% 
% % This block write the spatial and temporal summarize to csv files
 fno = char(strcat(str, '.csv'))
 csvwrite(fno,var);
 fno = char(strcat(str, '.summary.csv'))
 csvwrite(fno,v);
 
toc/60/60
beep