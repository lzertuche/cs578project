function routes = extraction(filename, top30, routes)

% Reading file
data = importdata(filename, ',', 1);
% length(data.data)
% Populate demand data in route-codes
for i = 1:length(data.data)
    for j = 1:length(top30.code)
        for k = 1:length(top30.code)
            if routes.id(j,k) == data.data(i,1)
                routes.dist(j,k) = data.data(i,2);
                routes.market(j,k) = data.data(i,10);
                routes.segment(j,k) = data.data(i,9);
            end
        end
    end
end

for i = 1:30
    for j = 1:30
%         keyboard
        if routes.market(i,j) == 0
            routes.market(i,j) = routes.market(j,i);
            routes.dist(i,j) = routes.dist(j,i);
            routes.segment(i,j) = routes.segment(j,i);
        end
    end
end
end