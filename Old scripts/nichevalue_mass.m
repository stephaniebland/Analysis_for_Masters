clear;

for i=1:100000
    clearvars -except i
    beep off
    warning off MATLAB:divideByZero;
    
    cd('/Users/JurassicPark/Google Drive/GIT/Masters Project')
    %to construct niche web, uncomment the following lines.
    S_0=30;
    connectance=0.15;
    [nicheweb,n_new,c_new,r_new] = NicheModel(S_0, connectance);%Create a connected (no infinite degrees of separation) foodweb with realistic species (eg. no predators without prey), and no isolated species.
    
    %or enter custom web (rows eats column)
    %nicheweb = [0 0 0; 1 0 0; 0 1 0];
    
    nichewebsize = length(nicheweb);%Steph: Find number of species (not sure why, already have S_0)
    basalsp = find(sum(nicheweb,2)==0);%List the autotrophs (So whatever doesn't have prey)  Hidden assumption - can't assign negative prey values (but why would you?)
    
    %%-------------------------------------------------------------------------
    %%  FIRST: SET DYNAMICS PARAMETERS
    %%-------------------------------------------------------------------------
    %Calculates species weight -> so you know how many life stages it needs
    %"meta", "TrophLevel" & "T1", "IsFish" and "Z"
    [TrophLevel,T1,T2]= TrophicLevels(nichewebsize,nicheweb,basalsp);
    [Z,Mvec,isfish]= MassCalc(nichewebsize,basalsp,TrophLevel);
    
    
    %no_plants = find(sum(nicheweb,2)~=0);
    %no_fish=find(1-isfish);
    %just_inve=intersect(no_plants,no_fish);
    isplant=zeros(S_0,1);
    isplant(basalsp)=1;
    either=isplant+isfish;
    webnumber=i*ones(length(n_new),1);
    log_mass=log(Mvec);
    log_ten=log10(Mvec);
    niche_mass=[webnumber, n_new, Mvec, log_mass,log_ten,isfish,isplant,either,TrophLevel,Z,T1',T2];
    
    
    %Save the file
    cd('/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files/Everything')
    dlmwrite(strcat('n_mass_',num2str(i),'.txt'),niche_mass,',') % export the nicheweb (to plot with network3d)
    
end

%%-------------------------------------------------------------------------
%%  FIND WEIGHT-NICHE VALUE RELATIONSHIP
%%-------------------------------------------------------------------------
%     logMvec=log10(Mvec);%take log because values are too big
%     plot(n_new,Mvec);
%     
%     fishMvec=Mvec(find(isfish));
%     inveMvec=Mvec(find(1-isfish));
%     fish_n=n_new(find(isfish));
%     inve_n=n_new(find(1-isfish));
%     hold on;
%     x=inve_n;%niche value is x axis
%     y=log10(inveMvec);%Mass is y axis
%     plot(inve_n,y,'o');
%     
%     
%     
%     %fit linear regression
%     X=[ones(length(x),1), x];
%     slope=X\y;
%     fitted_curve=X*slope;
%     plot(x,fitted_curve);
%     Rsq=1-sum((y - fitted_curve).^2)/sum((y - mean(y)).^2)








% 
%     %Do it again, but exclude plants
%     no_plants = find(sum(nicheweb,2)~=0);
%     no_fish=find(1-isfish);
%     just_inve=intersect(no_plants,no_fish);
%     
%     good_M=Mvec(just_inve);
%     good_n=n_new(just_inve);
%     hold on;
%     x=good_n;%niche value is x axis
%     y=log10(good_M);%Mass is y axis
%     plot(x,y,'o');
%     
%     %fit linear regression
%     X=[ones(length(x),1), x];
%     slope=X\y;
%     fitted_curve=X*slope;
%     plot(x,fitted_curve);
%     Rsq=1-sum((y - fitted_curve).^2)/sum((y - mean(y)).^2)
%     
%     