% q=0.0125;
% L_inf=28.4;
% K=0.37;
% t_0=-0.2;
% % Write Von-Bert Function
% 
% W_inf=q*(L_inf^3);
% W_inf*((1-exp(-K*(t-t_0)))^3);
% W=q*(L^growth_exp);
% L=(W/q)^(1/growth_exp);


W_max=500;
growth_exp=3;%Growth exponent, 3 is for isometric growth (Sangun et al. 2007)
q=0.0125;%Conversion factor from weight to length
t_max=0; %Age at maturity
L_max=(W_max/q)^(1/growth_exp);%(Sangun et al. 2007)
L_inf=(10^0.044)*(L_max^0.9841);
t=0:t_max*2;
K=3/t_max;% Set according to W_inf
t_0=t_max+((1/K)*log(1-(L_max/L_inf)));
L_t=L_inf*(1-exp(-K*(t-t_0)));%von-Bertalanffy growth model
% plot(t,L_t), hold on,
% plot(t,L_t,'o');
W_t=q*(L_t.^growth_exp);%(Sangun et al. 2007)
plot(t,W_t), hold on,
plot(t,W_t,'o');


W_inf=10^(0.044*growth_exp)*q^(1-0.9841)*W_max^0.9841;
t_max=4; %Age at maturity
K=0.5;%0.37;% Set according to W_inf
t_0=t_max+(1/K)*log(1-(W_max/W_inf)^(1/growth_exp));
t=0:t_max;
t=0:0.001:100;
W_t=W_inf*(1-exp(-K*(t-t_0))).^growth_exp;
% plot(t,W_t), hold on,
% plot(t_max,W_max,'o');
%Plot 4 stages
plot(t,W_t), hold on,
plot(t,W_t,'o');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

W_max=50;
growth_exp=3;
q=0.0125;
W_inf=10^(0.044*growth_exp)*q^(1-0.9841)*W_max^0.9841;
t_max=4; %Age at maturity
K=0.37;% Set according to W_inf
t_0=t_max+(1/K)*log(1-(W_max/W_inf)^(1/growth_exp));
t=0:t_max;
t=0:0.001:100;
%t=0:0.001:t_max*2;
W_t=W_inf*(1-exp(-K*(t-t_0))).^growth_exp;
% plot(t,W_t), hold on,
% plot(t_max,W_max,'o');
%Plot 4 stages
plot(t,W_t), hold on,
plot(t,W_t,'o');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 X=1:10;Y=randn(1,10);
 plot(X,Y), hold on,
 Marked=[X(2) Y(2); X(4)  Y(4)];
 plot(Marked(:,1),Marked(:,2),'o');

%Old stuff
syms L_max Length W_max K t_max t;

L_inf(L_max)=(10^0.044)*L_max^0.9841;
Weight(Length)=q*Length^growth_exp;

t_0(t_max,K,W_max,W_inf)=t_max+(1/K)*log(1-(W_max/W_inf)^(1/growth_exp));
W_t(W_inf,K,t,t_0)=W_inf*(1-exp(-K*(t-t_0)))^growth_exp;

q=0.0125;
L_inf=28.4;
K=0.37;
t_0=-0.2;



W_t()

%%%%%%%%%%%%%%%%%comments






function y = computeSquare(x)
y = x.^2;
end

f = @computeSquare;
a = 4;
b = f(a)




function y = average(x)
if ~isvector(x)
    error('Input must be a vector')
end
y = sum(x)/length(x); 
end

z = 1:99;
average(z)

% Compute the value of the integrand at 2*pi/3.
x = 2*pi/3;
y = myIntegrand(x)

% Compute the area under the curve from 0 to pi.
xmin = 0;
xmax = pi;
f = @myIntegrand;
a = integral(f,xmin,xmax)

function y = myIntegrand(x)
y = sin(x).^3;
end













syms L_max Length W_max K t_max t;
growth_exp=3;
q=0.0125;
L_inf(L_max)=(10^0.044)*L_max^0.9841;
Weight(Length)=q*Length^growth_exp;
W_inf(W_max)=10^(0.044*growth_exp)*q^(1-0.9841)*W_max^0.9841;
t_0(t_max,K,W_max,W_inf)=t_max+(1/K)*log(1-(W_max/W_inf)^(1/growth_exp));
W_t(W_inf,K,t,t_0)=W_inf*(1-exp(-K*(t-t_0)))^growth_exp;

q=0.0125;
L_inf=28.4;
K=0.37;
t_0=-0.2;



W_t()



