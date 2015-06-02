#!/bin/bash
#Bash commands for creating sample directories
#  for Round 2 of Real Estate Tax Experiment

cd /media/michael/MIKE_C/
sudo mkdir DoR_sample
cd DoR_sample
sudo mkdir amenities control duty lien moral peer sheriff
cd ..
sudo mkdir Lawler_sample
cd Lawler_sample
sudo mkdir amenities control duty lien moral peer sheriff
cd ..
cp amenities_template.docx round_2_sample_amenities_small_envelope.csv ./DoR_sample/amenities/
cp control_template.docx round_2_sample_control_small_envelope.csv ./DoR_sample/control/
cp duty_template.docx round_2_sample_duty_small_envelope.csv ./DoR_sample/duty/
cp liens_template.docx round_2_sample_lien_small_envelope.csv ./DoR_sample/lien/
cp moral_template.docx round_2_sample_moral_small_envelope.csv ./DoR_sample/moral/
cp peer_template.docx round_2_sample_peer_small_envelope.csv ./DoR_sample/peer/
cp sheriffs_sale_template.docx round_2_sample_sheriff_small_envelope.csv ./DoR_sample/sheriff/
cp amenities_template.docx round_2_sample_amenities_big_envelope.csv ./Lawler_sample/amenities/
cp control_template.docx round_2_sample_control_big_envelope.csv ./Lawler_sample/control/
cp duty_template.docx round_2_sample_duty_big_envelope.csv ./Lawler_sample/duty/
cp liens_template.docx round_2_sample_lien_big_envelope.csv ./Lawler_sample/lien/
cp moral_template.docx round_2_sample_moral_big_envelope.csv ./Lawler_sample/moral/
cp peer_template.docx round_2_sample_peer_big_envelope.csv ./Lawler_sample/peer/
cp sheriffs_sale_template.docx round_2_sample_sheriff_big_envelope.csv ./Lawler_sample/sheriff/
zip -r DoR_sample.zip DoR_sample
zip -r Lawler_sample.zip Lawler_sample
