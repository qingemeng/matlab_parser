%function [Ez, Ezx, Ezy, Hx, Hy] = splitter

    %Clearing variables in memory and Matlab command screen
%    clear all;
%    clc;

	pi = 3.14159;
    factor=6;
    %'factor' :- the number of times the smallest dimension (viz. 0.25 micron in 
    %             the given spec) is divided to get one space step (delta)
    % Increasing value of factor increases resolution of results but if that is done, 
    % to see full result, value of time_tot (total time steps) should be increased. 

    %Courant stability factor
    %S=1/(2.*0.5); octavexml does not parse this properly
    two = 2;
	S=1/(two^0.5);


    % Parameters of free space (permittivity and permeability and speed of
    % light) are all not 1 and are given real values
    epsilon0=(1/(36*pi))*1e-9;
    mu0=4*pi*1e-7;
    c=3e+8;

    % Spatial grid step length (spatial grid step= 0.25 micron/factor; and factor 
    % can be changed)
    delta=0.25e-6/factor;
    % Temporal grid step obtained using Courant condition
    deltat=S*delta/c;

    %Total no of time steps
    time_tot=1000;

    % Grid Dimension in x (xdim) and y (ydim) directions
    ydim=32*factor;%The domain is 30*factor space steps or 32*0.25=8 microns long
    xdim=80*factor;%The domain is 80*factor space steps or 80*0.25=20 microns wide

    %Free-space wavelength in microns
    wav=2;

    %Index of the three waveguides
    index=1.5;

    % Initialization of permittivity and permeability matrices
    epsilon=epsilon0*ones(xdim,ydim);
    mu=mu0*ones(xdim,ydim);


    % Defining of the permittivity profile of the region:-

    % Specifying bottom most waveguide location and dimensions i.e full length 
    % of the platform viz 20 microns and lying from 9*0.25=2.25 microns to 
    % 13*0.25=3.25 microns along the width with a dimension of 1 micron.
    epsilon(:,9*factor+1:13*factor)=index*index*epsilon0;

    % Specifying central waveguide location and dimensions i.e full length 
    % of the platform viz 20 microns and lying from 14*0.25=3.5 microns to 
    % 18*0.25=4.5 microns along the width with a dimension of 1 micron.
    epsilon(:,14*factor+1:18*factor)=index*index*epsilon0;

    % Specifying upper most waveguide location and dimensions i.e full length 
    % of the platform viz 20 microns and lying from 19*0.25=4.75 microns to 
    % 23*0.25=5.75 microns along the width with a dimension of 1 micron.
    epsilon(:,19*factor+1:23*factor)=index*index*epsilon0;


    % 2D FDTD update for PML boundary as used in previous program
    
    % Initialization of field matrices
    Ez=zeros(xdim,ydim);
    Ezx=zeros(xdim,ydim);
    Ezy=zeros(xdim,ydim);
    Hy=zeros(xdim,ydim);
    Hx=zeros(xdim,ydim);
    
    % Initializing electric conductivity matrices in x and y directions
    sigmax=zeros(xdim,ydim);
    sigmay=zeros(xdim,ydim);
    
    
    %Perfectly matched layer boundary design
    %Reference:-http://dougneubauer.com/wp-content/uploads/wdata/yee2dpml1/yee2d_c.txt
    %(An adaptation of 2-D FDTD TE code of Dr. Susan Hagness)
    
    %Boundary width of PML in all directions
    bound_width=factor*4;
    
    %Order of polynomial on which sigma is modeled
    gradingorder=6;
    
    %Required reflection co-efficient
    refl_coeff=1e-6;
    
    %Polynomial model for sigma
    sigmamax=(-log10(refl_coeff)*(gradingorder+1)*epsilon0*c)/(2*bound_width*delta);
    boundfact1=((epsilon(xdim/2,bound_width)/epsilon0)*sigmamax)/((bound_width^gradingorder)*(gradingorder+1));
    boundfact2=((epsilon(xdim/2,ydim-bound_width)/epsilon0)*sigmamax)/((bound_width^gradingorder)*(gradingorder+1));
    boundfact3=((epsilon(bound_width,ydim/2)/epsilon0)*sigmamax)/((bound_width^gradingorder)*(gradingorder+1));
    boundfact4=((epsilon(xdim-bound_width,ydim/2)/epsilon0)*sigmamax)/((bound_width^gradingorder)*(gradingorder+1));
    x=0:1:bound_width;
    for i=1:1:xdim
        sigmax(i,bound_width+1:-1:1)=boundfact1*((x+0.5*ones(1,bound_width+1)).^(gradingorder+1)-(x-0.5*[0 ones(1,bound_width)]).^(gradingorder+1));
        sigmax(i,ydim-bound_width:1:ydim)=boundfact2*((x+0.5*ones(1,bound_width+1)).^(gradingorder+1)-(x-0.5*[0 ones(1,bound_width)]).^(gradingorder+1));
    end
    for i=1:1:ydim
        sigmay(bound_width+1:-1:1,i)=boundfact3*((x+0.5*ones(1,bound_width+1)).^(gradingorder+1)-(x-0.5*[0 ones(1,bound_width)]).^(gradingorder+1))';
        sigmay(xdim-bound_width:1:xdim,i)=boundfact4*((x+0.5*ones(1,bound_width+1)).^(gradingorder+1)-(x-0.5*[0 ones(1,bound_width)]).^(gradingorder+1))';
    end
    
    %Magnetic conductivity matrix obtained by Perfectly Matched Layer condition
    %This is also split into x and y directions in Berenger's model
    sigma_starx=(sigmax.*mu)./epsilon;
    sigma_stary=(sigmay.*mu)./epsilon;
    
    %Multiplication factor matrices for H matrix update to avoid being calculated many times
    %in the time update loop so as to increase computation speed
    G=((mu-0.5*deltat*sigma_starx)./(mu+0.5*deltat*sigma_starx));
    H=(deltat/delta)./(mu+0.5*deltat*sigma_starx);
    A=((mu-0.5*deltat*sigma_stary)./(mu+0.5*deltat*sigma_stary));
    B=(deltat/delta)./(mu+0.5*deltat*sigma_stary);
    
    %Multiplication factor matrices for E matrix update to avoid being calculated many times
    %in the time update loop so as to increase computation speed
    C=((epsilon-0.5*deltat*sigmax)./(epsilon+0.5*deltat*sigmax));
    D=(deltat/delta)./(epsilon+0.5*deltat*sigmax);
    E=((epsilon-0.5*deltat*sigmay)./(epsilon+0.5*deltat*sigmay));
    F=(deltat/delta)./(epsilon+0.5*deltat*sigmay);
    
    % Update loop begins
    for n=1:1:time_tot
        
        %matrix update instead of for-loop for Hy and Hx fields
        Hy(1:xdim-1,1:ydim-1)=A(1:xdim-1,1:ydim-1).*Hy(1:xdim-1,1:ydim-1)+B(1:xdim-1,1:ydim-1).*(Ezx(2:xdim,1:ydim-1)-Ezx(1:xdim-1,1:ydim-1)+Ezy(2:xdim,1:ydim-1)-Ezy(1:xdim-1,1:ydim-1));
        Hx(1:xdim-1,1:ydim-1)=G(1:xdim-1,1:ydim-1).*Hx(1:xdim-1,1:ydim-1)-H(1:xdim-1,1:ydim-1).*(Ezx(1:xdim-1,2:ydim)-Ezx(1:xdim-1,1:ydim-1)+Ezy(1:xdim-1,2:ydim)-Ezy(1:xdim-1,1:ydim-1));
        
        %matrix update instead of for-loop for Ez field
        Ezx(2:xdim,2:ydim)=C(2:xdim,2:ydim).*Ezx(2:xdim,2:ydim)+D(2:xdim,2:ydim).*(-Hx(2:xdim,2:ydim)+Hx(2:xdim,1:ydim-1));
        Ezy(2:xdim,2:ydim)=E(2:xdim,2:ydim).*Ezy(2:xdim,2:ydim)+F(2:xdim,2:ydim).*(Hy(2:xdim,2:ydim)-Hy(1:xdim-1,2:ydim));
        
        % Source condition incorporating given free space wavelength 'wav'
        % and having a location at the left end of central waveguide just
        % after PML boundary
        tstart=1;
        N_lambda=wav*1e-6/delta;
        Ezx(bound_width,14*factor+1:18*factor)=0.5*sin(((2*pi*(c/(delta*N_lambda))*(n-tstart)*deltat)));
        Ezy(bound_width,14*factor+1:18*factor)=0.5*sin(((2*pi*(c/(delta*N_lambda))*(n-tstart)*deltat)));
                
%        Ez=Ezx+Ezy;
        
%         %Movie type colour scaled image plot of Ez
%         h=imagesc(delta*(1:1:xdim)*1e+6,(delta*(1:1:ydim)*1e+6)',Ez',[-1,1]);colorbar;
%         set(h,'AlphaData',10*epsilon'/epsilon0);
%         title(['\fontsize{20}Color-scaled image plot of Ez in waveguide splitter/combiner with PML boundary and at time = ',num2str(round(n*deltat*1e+15)),' fs']);
%         xlabel('x (in um)','FontSize',20);
%         ylabel('y (in um)','FontSize',20);
%         set(gca,'FontSize',20);
%         getframe;
    end
%end


% [1] "Silicon Photonics - An Introduction" - Graham. T. Reed, Andrew P Knights,
%      John Wiley & Sons, 2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END OF PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
