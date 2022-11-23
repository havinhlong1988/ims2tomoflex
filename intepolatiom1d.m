close all; clear all; clc;
% Interplote the velocities at the layers of 1d velocity model
fn="input1D";

Data = dlmread(fn);
nlay=Data(1,2);
d=Data(2,:);
v=Data(3,:);
vm=Data(4,1);
dm=Data(5,1);
% Build a model based on v and vm
% Detect the number of layers above dm
index=find(d<=dm);
index=index(1:end);
idx=max(index);
dnew = []; vnew = [];
dnew1 = []; vnew1 = [];
for i=index
    dnew = [dnew d(i) d(i)];
    vnew = [vnew v(i) v(i)];
end
dnew1=[dnew(2:end),dm,dm,dm,d(idx+1)]; vnew1=[vnew,v(idx),vm,vm];
dnew=[dnew(2:m),dm]; vnew=[vnew,vm];
% xq=-2.5:0.01:dm;
% vq = interpn(dnew,vnew,xq,'pchip');
% 
plot(vnew1,-dnew1,'b-','LineWidth',2);grid on; hold on;
% % plot(vnew1,-dnew1,'ro');
% plot(vq,-xq,'r-.');
% % 10k layer
% i10=find(xq==10);
% v10=vq(i10);
% % 20k layer
% i20=find(xq==20);
% v20=vq(i20);
% % 30k layer
% i30=find(xq==30);
% v30=vq(i30);
% % 40k layer
% i40=find(xq==30);
% v40=vq(i40);