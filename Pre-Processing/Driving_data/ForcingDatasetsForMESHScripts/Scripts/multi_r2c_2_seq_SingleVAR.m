%               Converting r2c multi-Frame Format to seq format 
%              Concatenating annual r2c files into one seq file
%                     By Mohamed Elshamy, May 2016

clear;
clc;

fprintf('%s\n', ' ***** Converting r2c to seq *****');
fprintf('%s\n', ' >>> PLEASE SEE THE NOTES IN THE CODE IF YOU NEED');

%variable names
VAR = {'huss','pr','ps','rlds','rsds','uva','ta'};
VAR1 = {'Humidity','Precip','Pressure','GLW','SW','Wind','T2'};
% processing one variable at a time - this is the variable index in VAR 
% used to construct file names
x = 2
F = 1;	% conversion factor - needed for precip only
if x == 2 F = 60*60;
ts = 'PGW'        % Use either 'CTL' or 'PGW' to process WRF-CTL or WRF-PGW
basin = 'KL'      % Basin ID for file naming

forcing_directory = char(strcat('D:\72_Yukon - Youssef\130_WRF_PGW_Updated\',VAR(x),'\'))

addpath(forcing_directory);
%load ('D:\MRB_Climate_Forcing\01_Princeton\R2C_Reprocessed\MRB_Mask.mat');
%MRB_Mask=flipud(MRB_Mask);

dir_data=dir(forcing_directory);
dir_index=[dir_data.isdir]';
file_list_all={dir_data(~dir_index).name}';
file_list=file_list_all(58:end);

str=char(strcat('Basin_',VAR1(x),'_'));       
%comparison=strncmp(str,file_list_all,13);

%file_list=file_list_all(x:end);

% Save the year no. from file list. The no. of seconds in new year file start from zero. 
%years_file_list=regexprep(file_list,str,'');
%years_file_list=regexprep(years_file_list,'.r2c','');
%years_file_list= cellfun(@(S) {S(1:end-5)}, years_file_list, 'UniformOutput', false);
%years_file_list=cellfun(@str2double, years_file_list);


% open the current MESH_drainage_database file
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\SRB_MESH_drainage_database_default.r2c','r');
%fid_dr = fopen('C:\WORK\03_MRB_Drainage_Databases\07_MRB_Drainage_Database_MEAME_Real_Lakes_12GRUsEW_Split_2005_v2.r2c','r');
fid_dr = fopen('D:\72_Yukon - Youssef\KluaneLake_GK-map-3_Shed_for-WATCH.r2c','r');
% Read the Header & Rank values
 
 
% read drainage database header 
tline = fgetl(fid_dr);
while strcmp(tline(1:10),':EndHeader')== 0 
%     fprintf(fid_w,'%s\n',tline);     
     tline = fgetl(fid_dr);
     if length(tline) < 20; tline = [tline blanks(20-length(tline))]; end
     if strcmp(tline(1:16),':TotalNumOfGrids')==1; num_grids=str2num(tline(17:end)); end  % num_grids is NO. of grids in the shd file
%     if strcmp(tline(1:11),':ClassCount')== 1; num_GRU=str2num(tline(12:end)); end % num_GRU is NO. of GRUs in the shd file
     if strcmp(tline(1:7),':xCount')==1; num_Col=str2num(tline(8:end)); end  % num_Col is NO. of rows in the shd file
     if strcmp(tline(1:7),':yCount')==1; num_Row=str2num(tline(8:end)); end  % num_Row is NO. of columns in the shd file
end
% fprintf(fid_w,'%s\n',tline); % To witre the word ":EndHeader" after exiting the loop
 
% Now read the rank matrix from drainage database - it needs to be
% transposed because fscanf reads columns first
Rank = fscanf(fid_dr,'%f',[num_Col num_Row]);
% Rank1 = transpose(Rank);
% Convert the Rank matrix and data matrix into vectors, then sort by Rank then remove all zeros
Rank1 = reshape(Rank,num_Col*num_Row,1);

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
 
% open seq file for writing
fno = char(strcat((VAR(x)),'_',basin,'_WRF_',ts,'_2000-2015'))
fid_seq = fopen([forcing_directory '..\' fno '.seq'],'w')

N = 1; %Frame Counter
Y1 = 2000; Y2 = 2015;
for y = Y1:Y2
  m1 = 1; m2 =12; f = 0;
  if y == 2015 
      m2 = 9;
  end    
  if y == 2000 
      m1 = 10;
      f = 273*24;
  end
  for m = m1:m2
    if m < 10 
        fid = [forcing_directory str num2str(y) '_0' num2str(m) '.r2c']
    else
        fid = [forcing_directory str num2str(y) '_' num2str(m) '.r2c']
    end
    fid_data=fopen(fid,'r');
    %display(fid);
    %fprintf('%d\n',N);
    
    % read data file header 
    tline = fgetl(fid_data);
    while strcmp(tline(1:10),':endHeader')== 0 
%        fprintf(fid_w,'%s\n',tline);     
        tline = fgetl(fid_data);
        if length(tline) < 20; tline = [tline blanks(20-length(tline))]; end
%     if strcmp(tline(1:11),':ClassCount')== 1; num_GRU=str2num(tline(12:end)); end % num_GRU is NO. of GRUs in the shd file
        if strcmp(tline(1:7),':xCount')== 1; num_Col_data=str2num(tline(8:end)); end  % num_Col is NO. of rows in the shd file
        if strcmp(tline(1:7),':yCount')== 1; num_Row_data=str2num(tline(8:end)); end  % num_Row is NO. of columns in the shd file
    end
% fprintf(fid_w,'%s\n',tline); % To witre the word ":EndHeader" after exiting the loop
 
    if num_Col_data ~= num_Col || num_Row_data ~= num_Row 
         fprintf('%s\n', 'data does not match drainage database');
         fclose(fid_dr);
         fclose(fid_data);
         exit
    end
    
 % Now read the data frames from r2c file

    while ~feof(fid_data)
        tline = fgetl(fid_data);    %should read frame info
        fprintf('%d %s\n',N,tline);
        if strcmp(tline(1:6), ':Frame') == 1
            k = strfind(tline, '"');  
            TimeStamp = tline(k(1)+1:end-1);
            Year = str2num(TimeStamp(1:4));
            Month = str2num(TimeStamp(6:7));
            Day = str2num(TimeStamp(9:10));
            Hour = str2num(TimeStamp(12:13));
            Minute = str2num(TimeStamp(15:16));
            Second = str2num(TimeStamp(18:19));
            %Millisecond =str2num(TimeStamp(21:23));
            Millisecond = 0;
        end %if
        %fprintf(fid_seq,'%s\n',tline);
        Data = fscanf(fid_data,'%f',[num_Col num_Row]);
        for i = 1:num_Col
            for j=1:num_Row
                if (Data(i,j) < 0.0 & Data(i,j) ~= -99.0) 
                    Data(i,j) = 0;
                end %if
            end %for j
        end %for i
        tline = fgetl(fid_data);    %reads end of line character
        tline = fgetl(fid_data);    %should read 'EndFrame:'
        % Convert the data matrix into a vector
        Data1 = reshape(Data,num_Col*num_Row,1)/F;
        % Put Rank and Data into a 2 column matrix
        X = [Rank1 Data1];
        % Sort in Ascending order by Rank
        B = sortrows(X,1);
        % Extract the active elements from the data
        SortedData = B(size(B,1)-ActiveElements+1:size(B,1),2);
        %fprintf(fid_seq,'%0.5g ',SortedData);
        %fprintf(fid_seq,'\n');
       
        seqHeader = [N N Year Month Day Hour Minute Second Millisecond]; 
        recordlength = 4*length(seqHeader);
        fwrite(fid_seq,recordlength,'int');     %this is required to be read in FORTRAN as unformatted binary file
        fwrite(fid_seq,seqHeader,'int');
        fwrite(fid_seq,recordlength,'int');
        
        recordlength = 4 * ActiveElements;
        fwrite(fid_seq,recordlength,'int');
        fwrite(fid_seq,SortedData,'real*4');
        fwrite(fid_seq,recordlength,'int');
        
        if (Day == 29 & Month == 2) %29 Feb on a Leap year
           %Do nothing
        else
           f = f + 1;
           var(f,y-Y1+1) = mean(SortedData);            
        end
        N = N + 1;
    end %while
    fclose(fid_data);
  end %for m
end %for y

fclose(fid_seq);
%  %This block further summarizes the spatial means temporally to the monthly
%time step
d = [31 28 31 30 31 30 31 31 30 31 30 31];
s = 1;e = 1; NT = 24;
for m = 1:12
     e = e + d(m) * NT;
     v(m,:) = mean(var(s:e-1,:));
     s = e;
end
% 
% % This block write the spatial and temporal summarize to csv files
fn1 = [forcing_directory '..\' fno '.csv']
csvwrite(fn1,var);
fn2 = [forcing_directory '..\' fno '.summary.csv']
csvwrite(fn2,v);
 
fprintf('%s\n', ' ************** SUCCESSFUL FINISH ***************');

