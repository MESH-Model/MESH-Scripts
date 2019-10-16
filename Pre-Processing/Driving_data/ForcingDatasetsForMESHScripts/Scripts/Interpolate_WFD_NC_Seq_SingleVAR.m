% Written by Mohamed Elshamy - Aug-Dec 2018
% This script reads netCDF data and writes seq files for a given basin
% Adapted to read and interpolate WFD data
% Refer to the README in the data folder to know how the dataset is organized
% The script processes all the series in a single seq file - it could be
% large, especially for large basins or high resolution grids
% but one can define the start and end years to process and if segmentation
% is needed, it can be automated by adding elements to vectors Y1 and Y2
% and using the p loop to go through them (lines 122, 124)
% Grid is perfectly Lat-Lon, thus interpolation is done using "intrep2" but
% the original data is organized as 1D vector so it is first mapped to 2D
% using coordinates in a seperate netcdf file
% Rainfall and Snowfall are provided seperately - MESH expects total
% precipitation, therefore they need to be summed
% inputs include MESH Drainage Database - line 44
% Path and naming pattern of input ncfiles is to be supplied on line 97
% with data selection on lines 25 and 29
% Path and naming pattern of seq files (to be created) is to be supplied on
% line 95. Folders have to be pre-created by the user  
% A sub-basin prefix is added and need to be supplied for your basin on
% line 41 

tic;    clc;    clear;  
VAR = {'LWdown','SWdown' 'PSurf','Qair','Rainf','Snowf','Tair','Wind'};
x = 7; % index of variable to be processed
str1=[char(VAR(x)) '_WFD'];

VAR1 = {'CRU','GPCC'};  
x1 = 2;  %index of precip variant if applicable, set only if x = 6 or 7, otherwise set to zero
if x1 > 0
    str=[char(VAR(x)) '_WFD_' char(VAR1(x1)) '_'];
else
    str = [str1 '_'];
end

% Number of time steps per day, it varies according to variables - check the dataset README
% Use 6 for LWdown, PSurf, Qair, Tair, Wind
% Use 3 for Rainf, Snowf, SWdown
T = 6;     

basin = 'KL';

% open the current MESH_drainage_database file
fid_dr = fopen('D:\72_Yukon - Youssef\KluaneLake_GK-map-3_Shed_for-WATCH.r2c','r');
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\OLD\05_MRB_Drainage_Database_MEAME_Real_Lakes_v4.r2c','r');
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
% 
xmodel = xmodel(1:end-1);
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

addpath(genpath('C:\Program Files\MATLAB\Tools\'));
Workspace_Path =(['D:\72_Yukon - Youssef\WFD\']);

forcing_file_directory = (['V:\giws_research_water_share\JeffersonW\0_Raw_Data\ClimateForcing\ClimateForcing_WATCH\']);
lat_lon_file = ([forcing_file_directory 'WFD-land-lat-long-z.nc']);
forcing_file_directory = ([forcing_file_directory 'WFD_05d3hr\' str1]);

%lat = ncread(lat_lon_file, 'Latitude');         
%lon = ncread(lat_lon_file, 'Longitude');
land = ncread(lat_lon_file, 'land');
glat = ncread(lat_lon_file, 'Grid_lat');
glon = ncread(lat_lon_file, 'Grid_lon');

xlon = -180+0.25:0.5:180-0.25;
ylat = -90+0.25:0.5:90-0.25;

addpath(forcing_file_directory);
dir_data=dir(forcing_file_directory);    dir_index=[dir_data.isdir]';    file_list_all={dir_data(~dir_index).name}';
comparison=strncmp(str,file_list_all,length(str));
file_list=file_list_all(comparison);

% Save the year no. from file list. The no. of seconds in new year file start from zero. 
years_file_list=regexprep(file_list,str,'');
years_file_list=regexprep(years_file_list,'.nc','');
years_file_list= cellfun(@(S) {S(1:end-2)}, years_file_list, 'UniformOutput', false);
years_file_list=cellfun(@str2double, years_file_list);
NT = 24/T;
formatout=('yyyy/mm/dd HH:MM:SS.000');
Y1 = [1951]; Y2 = [1960];
base = Y1 - 1;
for p=1:1
    % open seq file for writing
    str = [Workspace_Path str basin '_' num2str(Y1(p)) '_' num2str(Y2(p))]
    fn_seq = [str '.seq']
    fid_seq = fopen(fn_seq,'w');
        
    for y=Y1(p):Y2(p) %length(file_list)   %   yearly file counter. iterate through each yearly file 

        n = 1; f = 1;
        for m = 1 : 12
            z = (y-1901)*12+m
            file_list(z);
            fn = sprintf('%s',file_list{z})
            
%            lat = ncread(fn, 'lat');         
%            lon = ncread(fn, 'lon');
            time = ncread(fn,'time');
            Raw_Data = ncread(fn, char(VAR(x)));
%             x1 = int32((xOrigin - lon(1))/0.5);
%             y1 = int32((yOrigin - lat(1))/0.5);
%             
%             xC =int32(xCount*xDelta/0.5+1);
%             yC = int32(yCount*yDelta/0.5+1);

            for i=1:length(time)    
                elapsed_time=datestr(double(time(i))/24/60/60 + datenum(y,1,1,0,0,0));
                date_str=datestr(elapsed_time,formatout);   %   covert date to string format. the character length will be uniform in this format
                fprintf('%s\n',date_str);
                
                matx = zeros(360,720);
                for j = 1 : size(land)
                    matx(glat(j),glon(j))=Raw_Data(j,i);
                end
                %figure;imagesc(matx)
                matx = flipud(matx);
                %figure;imagesc(matx)
                Data=interp2(xlon,ylat,matx,xmodel,ymodel,'linear');    %   interpolate to xDelta and yDelta as given in the drainage database
                %figure;imagesc(flipud(Data))
                Year = y;
                Month = str2num(date_str(6:7));
                Day = str2num(date_str(9:10));
                Hour = str2num(date_str(12:13));
                Minute = str2num(date_str(15:16));
                Second = str2num(date_str(18:19));
                Millisecond =str2num(date_str(21:23));

                Data1 = reshape(Data(:,:),xCount*yCount,1);
                % Put Rank and Data into a 2 column matrix
                X = [Rank1 Data1];
                % Sort in Ascending order by Rank
                B = sortrows(X,1);
                % Extract the active elements from the data
                SortedData = B(size(B,1)-ActiveElements+1:size(B,1),2);
    
                seqHeader = [n n Year Month Day Hour Minute Second Millisecond]; 
                recordlength = 4*length(seqHeader);
                fwrite(fid_seq,recordlength,'int');     %this is required to be read in FORTRAN as unformatted binary file
                fwrite(fid_seq,seqHeader,'int');
                fwrite(fid_seq,recordlength,'int');

                recordlength = 4 * ActiveElements;
                fwrite(fid_seq,recordlength,'int');
                fwrite(fid_seq,SortedData,'real*4');
                fwrite(fid_seq,recordlength,'int');
                if mod(Year,4) == 0 & Month == 3 & Day == 1 & Hour == 0
                    f = f - NT;     %ignores Feb29 when caculating summaries
                end
                var(f,y-base) = mean(SortedData);
                n = n + 1;                
                f = f + 1;
            end
        end
    end
    fclose(fid_seq);
end

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
fno = [str '.csv']
csvwrite(fno,var);
fno = [str '.summary.csv']
csvwrite(fno,v);

fprintf('%s\n', ' ************** SUCCESSFUL FINISH ***************');
toc/60/60
beep
