%%
clear
close all
clc
%%
addpath('../../source')
import ORDERED_TEC.*
%%
x = 0:0.05:2*pi;
[x,y] = meshgrid(x);
u = sin(x).*cos(y);
v = cos(x)+sin(y);
%%
tec_file = TEC_FILE;
tec_file.FileName = 'test_10';
tec_file.Variables = {'x','y','u','v'};
tec_file.Auxiliary = {{'a1','1'},{'a2','2'}};
tec_file.Zones = TEC_ZONE;
tec_file.Zones.Data = {x,y,u,v};
%%
tec_file = tec_file.write_plt();
tec_file.last_log.write_echo();
tec_file.last_log.write_json();
tec_file.last_log.write_xml();
%%
logfile = TEC_FILE_LOG;
doc = xmlread('test_10.xml');
logfile = logfile.read_xml(doc.getDocumentElement);

disp(logfile)
disp(logfile.Zones)
disp(logfile.Zones.Data)
