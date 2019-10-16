% This script reads netCDF data and writes seq files for a given basin
% Written by Mohamed Elshamy - Jan 2017 - Dec 2018
% inputs include MESH Drainage Database - line 27
% Path and naming pattern of ncfiles is to be supplied on lines 90-91, 95-96 
% Path and naming pattern of seq files (to be created) is to be supplied on
% lines 92-93, 97-98 
% Atapted to read the latest WRF bias corrected data
% WRF data is only 15 years, thus it is not segmented, select whether you
% are processing either CTL or PGW run
% this script does not interpolate, it just clips the data - suitable for
% basins where the grid is aligned and has the same resolution as the bias
% corrected WRF data. Refer to the bias corrected WRF data README for
% information about the grid and variables
% Output folders have to pre-created by the Use

tic;    clc;    clear;
%variable names
VAR = {'hus','pr','ps','rlds','rsds','uva','ta'};
% processing one variable at a time - this is the variable index in VAR 
% used to construct file names
x = 1
ts = 'pgw'         % change to 'pgw' to process WRF-PGW
basin = 'SRB'      % Basin ID for file naming
% ==========================================================================================================
% open the current MESH_drainage_database file - all what is needed is RANK
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\07_MRB_Drainage_Database_MEAME_Real_Lakes_12GRUsEW_Split_2005_v2.r2c','r');
fid_dr = fopen('D:\70_SRB\SRB_MESH_drainage_database_default.r2c','r');
%fid_dr = fopen('D:\Yukon - Youssef\KluaneLake_GK-map-3_Shed_for-WATCH.r2c','r');
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

% xOrigin = xOrigin + xDelta/2;
% yOrigin = yOrigin + yDelta/2;
% 
% yCount = yCount - 4;
% xmodel = xOrigin:xDelta:(xOrigin+xDelta*xCount);
% ymodel = yOrigin:yDelta:(yOrigin+yDelta*yCount);
% % 
% xmodel = xmodel(1:end-1);
% ymodel = ymodel(1:end-1);
% [xmodel,ymodel]=meshgrid(xmodel,ymodel);

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
%====== Done with the draiange database file =============================

% P  is lists the start of time periods - the last value is the end of the
P = [2000, 2015];

T = 1; NT = 24/T;       % T is is length of the time step and NT is the number of time steps per day
SortedData = zeros(ActiveElements,NT);
    
recordlength1 = 4 * 9;
recordlength2 = 4 * ActiveElements;
formatout=('yyyy/mm/dd HH:MM:SS.000');

if ts == 'ctl'
    SourceDir = 'V:\Model_Output\WRF\WRF4KM_CA\124_WRF-CTL_Corrected_to_GEM-CaPA_monthly_MBCn_it200\';
    fn = char(strcat(SourceDir,VAR(x),'_MBCn_it200_20001001_20150927', ts, '.nc'))
    OutDir = ['E:\WRF\124_' basin '_WRF_CTL_MBCn_it200_GEM-CaPA-COR\'];
    str = strcat(OutDir,VAR(x),'_',basin,'_WRF_CTL_GEM-Cor_20001001_20150927');
elseif ts == 'pgw'
    SourceDir = 'V:\Model_Output\WRF\WRF4KM_CA\134_WRF-PGW_Corrected_to_GEM-CaPA_monthly_MBCn_it200\';
    fn = char(strcat(SourceDir,VAR(x),'_MBCn_it200_20001001_20150930', ts, '.nc'))
    OutDir = ['E:\WRF\134_' basin '_WRF_PGW_MBCn_it200_GEM-CaPA-COR\'];
    str = strcat(OutDir,VAR(x),'_',basin,'_WRF_PGW_GEM-Cor_20001001_20150930');
end

lat = ncread(fn, 'lat');        
lon = ncread(fn, 'lon');    
time = ncread(fn,'time');        %Mutiply by 24 if the time axis is daily

fno = char(strcat(str,'.seq'))
fio_seq = fopen(fno,'w');
if x == 2
	F = 60*60
else
	F = 1
end
for p = 1 : size(P,2)-1    
    
    % These need to be checked in case the model grid is not aligned with
    % that of the nc data
    x1 = int32((xOrigin - lon(1))/xDelta);
    y1 = int32((yOrigin - lat(1))/yDelta);

    n = 1; f = 273*NT+1; old=P(1);      % Frame Counters - they are the same if the data contains Feb29
    %Feb29 = 0;          % Feb29 counter - not needed if the data contains Feb29
    for i = 1 : size(time,1)

        %Data(:,:,:) = ncread(fn, x,[x1 y1 f], [xCount yCount NT]);  %CanRCM 3D data
        Data(:,:,:) = ncread(fn, char(VAR(x)),[x1 y1 i], [xCount yCount 1]);  %WRF-GEM-COR 3D data
        %if mod(f-1,365*NT)==0 
        %    g = 1;
        %end
        % activate plotting  to make sure the data is read correctly
        %figure; imagesc(flipud(Data(:,:)'))
        %for j = 1: NT
            % construct the date-time stamp - avoids descrepancies in the
            % nc files (like 22:59)
            % This needs testing whether Feb29 need be included or not
            % depedning on the time axis of the data 
            elapsed_time = (time(i)-time(1))/24 + datenum(P(1),10,1,0,0,0);    
            [Year, Month, Day, Hour, Minute, Second]=datevec(elapsed_time);   
            date_str=datestr(elapsed_time,formatout);   %   covert date to string format. the character length will be uniform in this format
            %fprintf('%s\n',date_str);
            
            seqHeader = [n n Year Month Day Hour Minute Second 0]; 
            if Year > old 
                old = Year;
                f = 1;
            end
            fwrite(fio_seq,recordlength1,'int');     %this is required to be read in FORTRAN as unformatted binary file
            fwrite(fio_seq,seqHeader,'int');
            fwrite(fio_seq,recordlength1,'int');

            Data1 = reshape(Data(:,:)',xCount*yCount,1)/F;
            % Put Rank and Data into a 2 column matrix
            X = [Rank1 Data1];
            % Sort in Ascending order by Rank
            B = sortrows(X,1);
            % Extract the active elements from the data
            SortedData = B(size(B,1)-ActiveElements+1:size(B,1),2);

            fwrite(fio_seq,recordlength2,'int');
            fwrite(fio_seq,SortedData,'real*4');
            fwrite(fio_seq,recordlength2,'int');

            % Activate var to calculate the spatial mean over the basin -
            % it is seperated by year to facilitate temporal averaging at
            % the end
            if mod(Year,4) == 0 & Month == 3 & Day == 1 & Hour == 0
                f = f - 24;
            end
            var(f,Year-P(1)+1) = mean(SortedData);  
            fprintf('%i %i-%i-%i:%i\n',seqHeader(1),seqHeader(3),seqHeader(4),seqHeader(5),seqHeader(6));
            %var(f,int32(Year-1950)) = mean(SortedData(:,j));
            n = n + 1;
            f = f + 1;
            %g = g + 1;
              
        %end
        
% The following block is required if Feb29 is not in the dataset - it does not apply to GEM
%          Leap = 0;
%          if Month == 2 & Day == 28 %29 Feb may be coming, check if it is a leap year            
%              if mod(Year,4) == 0 
%                  if mod(Year,100) == 0 
%                      if mod(Year,400) == 0
%                          Leap = 1;
%                      end
%                  else
%                      Leap = 1;
%                  end
%              end
%          end           
%          if Leap == 1  %Leap year is coming which is not in file, repeat data for 28 Feb             
%             Feb29 = Feb29 + 1;
%             for j = 1:NT
%                seqHeader = [n n Year Month Day+1 (j-1)*T Minute Second 0]; 
%                
%                %for x=1:size(VAR,2)               
%                fwrite(fio_seq,recordlength1,'int');     %this is required to be read in FORTRAN as unformatted binary file
%                fwrite(fio_seq,seqHeader,'int');
%                fwrite(fio_seq,recordlength1,'int');
%                fprintf('%i %i-%i-%i:%i\n',seqHeader(1),seqHeader(3),seqHeader(4),seqHeader(5),seqHeader(6));
% 
%                fwrite(fio_seq,recordlength2,'int');
%                fwrite(fio_seq,SortedData(:,j),'real*4');
%                fwrite(fio_seq,recordlength2,'int');                               
% 
%                 n = n + 1; 
%              end
%         end
    end
end
fclose('all');close('all');

%  %This block further summarizes the spatial means temporally to the monthly
 %time step
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