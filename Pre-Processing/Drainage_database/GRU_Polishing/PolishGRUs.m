
%                     MESH polishing GRU per cell 
%                     By Amin Haghnegahdar May. 2012 

%   This code is written to Polish the MESH_drainage_database.r2c file by
%   removing the GRUs with a fraction smaller than a user-specified threshold   
%   from each grid cell starting from the 1st GRU, 2nd and so on. 
%   The removed portion will be re-distributed over the rest of GRUs
%   proportional to their areas. This polishing is specially useful for 
%   calibration purposes and could potentially result in significant runtime 
%   saving while still giving similar results (it is up to the user to test 
%   and decide what threshold is appropriate in this respect.)

%   Requirements: the original MESH_drainage_database.r2c file in the same directory 

clear;
clc;
Thr_Frac=0.05; %Threshold fraction (0 to 1) below which GRUs will be deleted 


fprintf('%s\n', ' ***** Polishing MESH drainage file for GRUs *****');
fprintf('%s\n', ' >>> PLEASE SEE THE NOTES IN THE CODE IF YOU NEED');

% Open the MESH_drainage_database.r2c file to write into
fid_w = fopen('MESH_drainage_database_Polished.r2c','w');
  
% open the current MESH_drainage_database file
% Read the Header & needed values and write into the output file
 fid_r = fopen('MESH_drainage_database.r2c','r');
 tline = fgetl(fid_r);
 while strcmp(tline(1:10),':EndHeader')== 0 
     fprintf(fid_w,'%s\n',tline);     
     tline = fgetl(fid_r);
     if strcmp(tline(1:11),':ClassCount')== 1; num_GRU=str2num(tline(28:32)); end % num_GRU is NO. of GRUs in the shd file
     if strcmp(tline(1:7),':xCount')== 1; num_Col=str2num(tline(28:32)); end  % num_Col is NO. of rows in the shd file
     if strcmp(tline(1:7),':yCount')== 1; num_Row=str2num(tline(28:32)); end  % num_Row is NO. of columns in the shd file
 end
 fprintf(fid_w,'%s\n',tline); % To witre the word ":EndHeader" after exiting the loop
 
 % Now read and write the unchanged lines
 for line_num=1:(12*num_Row)  % for file Header and all non-GRU info
     tline = fgetl(fid_r);
     fprintf(fid_w,'%s\n',tline);
 end
 
 
% printing number of GRUs, Rows & Columns for check
%fprintf('%s','NO. of GRUs=','%f\n','num_GRU');

GRU_fr=zeros(num_GRU,num_Row,num_Col);
dom_fr=zeros(num_GRU,num_Row,num_Col);
dom_GRU=zeros(num_GRU,num_Row,num_Col);

% Now start reading and editing the GRU information 
for k=1:num_GRU
    for i=1:num_Row
        tline = fgetl(fid_r);
        GRU_fr (k,i,:)=cell2mat(textscan(tline,'%f'));
    end
end

% Now start deleting GRU fractions below Thr_Frac
for j=1:num_Col
    for i=1:num_Row
        for k=1:num_GRU          % Finding the Max GRU fraction
            if (GRU_fr(k,i,j)>0 && GRU_fr(k,i,j)< Thr_Frac)
                add_frac= GRU_fr(k,i,j)/(sum(GRU_fr(:,i,j))-GRU_fr(k,i,j));
                for n=1:num_GRU
                    if n~= k
                        GRU_fr(n,i,j)= GRU_fr(n,i,j)*(1+add_frac); 
                    else
                        GRU_fr(n,i,j)=0;
                    end
                end               
            end
        end
    end
end

% Now write the new values to new file
for k=1:num_GRU
    for i=1:num_Row
        %for j=1:num_Col
            fprintf(fid_w, ' %4.3f', GRU_fr(k,i,:));
            fprintf(fid_w, '%s \n','');
        %end
    end
end
for i=1:num_Row  % for the unused GRU
    tline = fgetl(fid_r);
    fprintf(fid_w,'%s \n',tline);
end
      
fclose(fid_r);
fclose(fid_w);

fprintf('%s\n', ' ************** SUCCESSFUL FINISH ***************');

