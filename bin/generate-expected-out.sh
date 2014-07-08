#!/bin/sh

cd data

cat b12.dat | java vehicle > expected-b12-vehicle.out
cat b12t0.dat | java vehicle > expected-b12t0-vehicle.out
cat bm.dat | java vehicle > expected-bm-vehicle.out
cat np.dat | java vehicle > expected-np-vehicle.out
cat o.dat | java vehicle > expected-o-vehicle.out
cat sp.dat | java vehicle > expected-sp-vehicle.out

cat expected-b12-vehicle.out | java satellite > expected-b12-satellite.out
cat expected-b12t0-vehicle.out | java satellite > expected-b12t0-satellite.out
cat expected-bm-vehicle.out | java satellite > expected-bm-satellite.out
cat expected-np-vehicle.out | java satellite > expected-np-satellite.out
cat expected-o-vehicle.out | java satellite > expected-o-satellite.out
cat expected-sp-vehicle.out | java satellite > expected-sp-satellite.out

cat expected-b12-satellite.out | java receiver > expected-b12-receiver.out
cat expected-b12t0-satellite.out | java receiver > expected-b12t0-receiver.out
cat expected-bm-satellite.out | java receiver > expected-bm-receiver.out
cat expected-np-satellite.out | java receiver > expected-np-receiver.out
cat expected-o-satellite.out | java receiver > expected-o-receiver.out
cat expected-sp-satellite.out | java receiver > expected-sp-receiver.out
