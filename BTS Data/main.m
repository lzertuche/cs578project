%% Demand Data extraction for CS 578 Project
% Code Author: Apoorv Maheshwari

clear all

%% Reading data
list = importdata('airportsList.csv',',',1);
faa_list = importdata('cy14-commercial-service-enplanements.xlsx',1);

%% Creates a structure for Top30 Airports with code, id and hub
for i = 1:1:30
    top30.code(i) = faa_list.textdata(i+1,4);
end

for j = 1:length(top30.code)
    for i = 1:length(list.textdata)-1
        if list.textdata{i+1,1} == top30.code{j}
            top30.id(j) = list.data(i,1);
            top30.hub(j) = list.data(i,2);
        end
    end
end    

%% Create route-codes
for i = 1:length(top30.code)
    for j = 1:length(top30.code)
        temp = [num2str(top30.id(i)) num2str(top30.id(j))];
        routes.id(i,j) = str2double(temp);
    end
end

%% Populate demand for each year by calling the extraction function
for i = 2:14
    filename = sprintf('finalData20%02i.csv',i);
    year = sprintf('y20%02i',i);
    demand.(year) = extraction(filename, top30, routes);
end