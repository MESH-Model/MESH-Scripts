function MESHstreamflow(keyssf,str_datef,end_datef)
% Created by Fuad Yassin    fuad.yassin@usask.ca
% to run this function use, provide the station information file and starting and end period
%  example below creates mesh streamflow file starting from '2002/01/01' up to '2016/12/31'
% MESHstreamflow('RiverGauges_desnew.csv','2002/01/01','2016/12/31')
%
% keyssf = 'RiverGauges_des2.csv' % is the name of the dictionary file
% str_datef = '2002/01/01'; % end_datef = '2016/12/31';
my_dir = 'F:\A_Mywork\A_Data\B_Flowdata\SakRB_final\flow' ;
cd(my_dir)
addpath(genpath(my_dir))
keyss = readtable(keyssf);
% Number of stations exist in the keyss
siz = length(keyss.Station(1:end));
% This collects all the station names
vars = {keyss.Station(1:end)};
stalist =(vars{1,1})';
S = [];
% This one put import all stations data from CSV files and put them in a
% structure if error exists most of the time it is becaue of the last row
% lastcolumn variable add 'B' on it
for i=1:1:siz
    stationname=keyss.Station{i};
    %%
    % Change the location of the streamflow file (Directory)
    %%
    prodir_0 = my_dir;    % change based on you case
    proname = ['05',stationname,'_Daily_Flow_ts.csv']; %
    filelocc =[prodir_0,'\',proname];
    filelocc = strcat(filelocc)
    S.(stalist{i})=readtable(filelocc,'Delimiter',',');
end
% check always the date format of the csv file
formatIn = 'yyyy/mm/dd';
for i=1:1:siz
    A{i}=[datenum(S.(stalist{i}).Date(1:end),formatIn),S.(stalist{i}).Flow(1:end)];
end
mintt=[];
maxtt=[];
for i=1:1:siz
    minntime=min(A{1,i}(:,1)); maxxtime=max(A{1,i}(:,1));
    mintt=[mintt;minntime]; maxtt=[maxtt;maxxtime];
end


startttime2  = datenum(str_datef,formatIn);
enddtime2  = datenum(end_datef,formatIn);

startttime=min(min(mintt(:,1)),startttime2); enddtime=max(max(maxtt(:,1)),enddtime2);
allin=nan((enddtime-startttime+1),(siz+1));
jjj=[startttime:1:enddtime]';
allin(:,1)=jjj(:,1);
% it starts with two because the first column is time
for ii=2:1:(siz+1)
    sttt=find(allin(:,1)==mintt((ii-1),1));
    enddd=find(allin(:,1)==maxtt((ii-1),1));
    allin(sttt:enddd,ii)=A{1,(ii-1)}(:,2);
end
%% 
%change the starting date, ending dat......
%%
str_date=str_datef; end_date=end_datef;  % modify based on your case
fufuvec=datevec(str_date);
star_year=fufuvec(1,1); star_day=01; star_hour=00;      % modify based on your case
str_obs=find(allin(:,1)==datenum(str_date,formatIn));
end_obs=find(allin(:,1)==datenum(end_date,formatIn));
flowall=allin(str_obs:end_obs,2:end);
% flowall(:,2) = flowall(:,3).*0.15;
% flowall(:,1) = flowall(:,3).*0.5;

%% 
flowall(isnan(flowall))=-1;

%% write streamflow file
keysprint=[(60*keyss.Latitude(1:end)),(60*keyss.Longitude(1:end))];
latf =    [keyss.Longitude(1:end),keyss.Latitude(1:end),keyss.DrainageArea(1:end)];
filename = [my_dir,'\','MESH_input_streamflow',datestr(now,'ddmmmyy-HH-MM'),'.txt'];
fileID = fopen(filename,'w');
fprintf(fileID,'%s\t','Observedstreamflow');    fprintf(fileID,'%s\t',str_datef);    fprintf(fileID,'%s\n',end_datef);
fprintf(fileID,'%6d %6d %6d %4d %4d %4d %02d\n',siz,length(flowall(:,1)),length(flowall(:,1)),24,star_year,star_day,star_hour);
for jj = 1:size(keysprint,1)
    stationid=char(keyss.Station{jj});
    namef  =char(keyss.StationName{jj});
    stationID=['05',stationid];
    fprintf(fileID,'%4.0f\t',keysprint(jj,:));
    fprintf(fileID,'%s\t',stationID);
    fprintf(fileID,' %10.3f\t',latf(jj,:)); 
    fprintf(fileID,'%4d\t',jj);
    fprintf(fileID,'%s',namef);
    fprintf(fileID,'\n');
end
for ii = 1:size(flowall,1)
    fprintf(fileID,'%10.3f\t',flowall(ii,:));
    fprintf(fileID,'\n');
end
fclose(fileID);
end