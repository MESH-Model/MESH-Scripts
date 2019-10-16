% Written by Mohamed Elshamy - Aug 2017 - Dec 2018
% This script reads netCDF data and writes seq files for a given basin
% Adapted to read WFDEI data as processed for NA domain by Elvis Asong
% Refer to the README in the data folder to know how the dataset is organized
% Grid is perfectly alighned for MRB, thus no interpolation needed
% inputs include MESH Drainage Database - line 18
% Path and naming pattern of ncfiles is to be supplied on line 62
% Path and naming pattern of seq files (to be created) is to be supplied on
% line 64. Folders have to be pre-created by the user.

tic;    clc;    clear;              
VAR = {'huss','pr','ps','rlds','rsds','sfcWind','tas'};
x = 5
basin = 'Simonette'

% open the current MESH_drainage_database file
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\07_MRB_Drainage_Database_MEAME_Real_Lakes_12GRUsEW_Split_2005_v2.r2c','r');
%fid_dr = fopen('D:\51_Red Deer\RedDeer\GEM_CaPA\MESH_drainage_database.r2c','r');
fid_dr = fopen('D:\50_Simonette\Simonette\GEM_CaPA\MESH_drainage_database.r2c','r');
%fid_dr = fopen('D:\71_Souris\UofM_MESH_drainage_database.r2c','r');
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

xOrigin = xOrigin + xDelta/2;
yOrigin = yOrigin + yDelta/2;

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
%======================= Done with Drainage Database =====================

fn = char(strcat('V:\Model_Output\181_WFDEI-GEM_1979_2016\', VAR(x), '_WFDEI_GEM_1979_2016-final_thresholded.nc'))
%fn = 'Z:\ClimateForcing_Data\ClimateForcing_WFDEI\WFDEI_05d3hr\Rainf_WFDEI_CRU\Rainf_WFDEI_CRU_197901.nc'
str = strcat('D:\50_Simonette\181_WFDEI-GEM_1979-2016\', basin, '_', VAR(x), '_WFDEI-GEM_1979-2016_thresholded');
fno = char(strcat(str, '.seq'))
fio_seq = fopen(fno,'w');

lat = ncread(fn, 'lat'); % + yDelta/2;         
lon = ncread(fn, 'lon'); % + xDelta/2;
%x = size(lat,1);
%y = size(lon,1);
% uncomment the division in case of WFDEI because it has time axis in
% seconds rather than hours
time = ncread(fn,'time'); %/60/60;      

x1 = int32((xOrigin - lon(1))/xDelta);
y1 = int32((yOrigin - lat(1))/yDelta);

x2 = (xOrigin - lon(1))/xDelta;
y2 = (yOrigin - lat(1))/yDelta;

formatout=('yyyy/mm/dd HH:MM:SS.000');
%Year = 1979; Month = 1;
T = 3; NT = 24/T;

%VAR = {'pr','ps','rlds','rsds','sfcWind','tas'};-14
%VAR = ('tas')
%for x = 1:size(VAR,2)  

%end
n = 1; f = 1; Feb29 = 0;        % Frame Counters
base = 1978;
recordlength1 = 4 * 9;
recordlength2 = 4 * ActiveElements;
        
for i = 1 : NT : size(time)
%    for x = 1:size(VAR,2)
        Data(:,:,:) = ncread(fn, char(VAR(x)),[x1 y1 f], [xCount yCount NT]);  
%    end
%    figure
%    imagesc(flipud(MRB(:,:,1,1)'))
    if mod(f-1,365*NT)==0
        g = 1;
    end
    for j = 1: NT
        elapsed_time = ((time(i)-time(1)) + (j-1) * T)/24 + Feb29 + datenum(base+1,1,1,0,0,0);    
        [Year, Month, Day, Hour, Minute, Second]=datevec(elapsed_time);   
        date_str=datestr(elapsed_time,formatout);   %   covert date to string format. the character length will be uniform in this format
%        fprintf('%s\n',date_str);

        seqHeader = [n n Year Month Day Hour Minute Second 0]; 
        
        %for x=1:size(VAR,2)
        fwrite(fio_seq,recordlength1,'int');     %this is required to be read in FORTRAN as unformatted binary file
        fwrite(fio_seq,seqHeader,'int');
        fwrite(fio_seq,recordlength1,'int');
        
        %Data = flipud(MRB(:,:,j)');
        %figure; imagesc(Data);
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
            
        var(g,Year-base) = mean(SortedData(:,j));  
        fprintf('%i %i-%i-%i:%i\n',seqHeader(1),seqHeader(3),seqHeader(4),seqHeader(5),seqHeader(6));
        
        n = n + 1;
        f = f + 1;
        g = g + 1;
    end
    if mod(Year,4) == 0 & Month == 2 & Day == 28 %29 Feb on a Leap year is coming which is not in file, repeat data for 28 Feb    
       Feb29 = Feb29 + 1
       for j = 1:NT
           seqHeader = [n n Year Month Day+1 (j-1)*T Minute Second 0]; 
           recordlength = 4*length(seqHeader);
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

d = [31 28 31 30 31 30 31 31 30 31 30 31];
s = 1;e = 1;
for m = 1:12
    e = e + d(m) * 24/T;
    v(m,:) = mean(var(s:e-1,:));
    s = e;
end
fno = char(strcat(str, '.csv'))
csvwrite(fno,var)
fno = char(strcat(str, '.summary.csv'))
csvwrite(fno,v)