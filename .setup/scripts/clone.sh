#!/bin/bash

git clone git@github.com:jeksterslab/fitDTVARMxID.git
rm -rf "$PWD.git"
mv fitDTVARMxID/.git "$PWD"
rm -rf fitDTVARMxID
