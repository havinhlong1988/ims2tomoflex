#! usr/bin/bash
rm -f ims2tomoflex
gfortran ims2tomoflex.f90 -o ims2tomoflex
./ims2tomoflex