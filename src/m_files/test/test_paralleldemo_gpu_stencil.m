function test_paralleldemo_gpu_stencil()
gridSize = 500;
numGenerations = 100;
initialGrid = (rand(gridSize,gridSize)>.75);
gpu = gpuDevice();

 % draw the initial grid
 % hold off
 imagesc(initialGrid)
 colormap([1, 1, 1;0, 0.5, 0])
 title('Initial Grid')


 grid = initialGrid;
 for generation = 1:numGenerations
     grid = updateGrid(grid, gridSize);

     imagesc(grid);
     title(num2str(generation));
%       drawnow;
end
 end

 function  [X] = updateGrid(X,N)
 p = [1, 1:N-1];
 q = [2:N, N];

 neighbors = X(:,p) + X(:,q) + X(p,:) + X(q,:) + X(p,p) + X(q,q) + X(p,q) + X(q,p);
 %         neighbors
 %        m = (neighbors==2)
 %        m
 
 X = (X && (neighbors == 2)) ||
  (neighbors == 3);
 % X
 end