%macro AddVariable(in_ds_name, out_ds_name, fao_class_name, lc1_name, lu1_name, lc2_name, lu2_name, lc1_species_name, survey_area_size_name,
survey_tree_height_maturity_name, survey_feature_width_name, survey_lc_lu_special_remark_name);

data &out_ds_name;
set &in_ds_name;
&fao_class_name='';

length condition_fao_class $5.;
length all_values $100.;

all_values=trim(compress(&lc1_name))||'_'||trim(compress(&lu1_name));
all_values=trim(compress(all_values))||'_'||trim(compress(&lc2_name));
all_values=trim(compress(all_values))||'_'||trim(compress(&lu2_name));
all_values=trim(compress(all_values))||'_'||trim(compress(&lc1_species_name));
all_values=trim(compress(all_values))||'_'||trim(compress(&survey_area_size_name));
all_values=trim(compress(all_values))||'_'||trim(compress(&survey_tree_height_maturity_name));
all_values=trim(compress(all_values))||'_'||trim(compress(&survey_feature_width_name));

/* Rule 1 */
if (upcase(substr(&lc1_name,1,1))='G' or upcase(substr(&lc1_name,1,1))='H') then do;
	&fao_class_name='0';
	condition_fao_class='1';
end;

/* Rule 2 */
if (upcase(&lc1_name)='A22' and upcase(&lu1_name)='U312' and upcase(&lu2_name)='U120' and &fao_class_name='') then do;
	&fao_class_name='1';
	condition_fao_class='2';
end;

/* Rule 3 */
if (upcase(&lc1_name)='A30' and 
(upcase(substr(&lc2_name,1,1))='C' or upcase(substr(&lc2_name,1,1))='D' or upcase(substr(&lc2_name,1,1))='E' or 
upcase(substr(&lc2_name,1,1))='F') and
upcase(&lu1_name)='U319' and upcase(&lu2_name)='U120' and &fao_class_name='') then do;
	&fao_class_name='1';
	condition_fao_class='3';
end;

/* Rule 4 */
if (upcase(substr(&lc1_name,1,2))='B7' and 
(upcase(&lu1_name)='U111' or upcase(&lu1_name)='U112' or upcase(&lu1_name)='U113' or upcase(substr(&lu1_name,1,2))='U4') and 
(upcase(&lc1_species_name) ne 'B75E' and upcase(&lc1_species_name) ne 'B75P') and 
&survey_area_size_name>1 and &fao_class_name='') then do;
	&fao_class_name='3';
	condition_fao_class='4';
end;

/* Rule 5 */
if (upcase(&lc1_name)='B81' and 
(upcase(&lu1_name)='U111' or upcase(&lu1_name)='U112' or upcase(&lu1_name)='U113' or upcase(substr(&lu1_name,1,2))='U4') and 
&survey_area_size_name>1 and &fao_class_name='') then do;
	&fao_class_name='3';
	condition_fao_class='5';
end;

/* Rule 6 */
if (upcase(&lc1_species_name)='B83F' and &survey_area_size_name>1 and &fao_class_name='') then do;
	&fao_class_name='1';
	condition_fao_class='6';
end;

/* Rule 7 */
if (upcase(substr(&lc1_name,1,1))='C' and &lc2_name='8' and upcase(&lu1_name)='U111' and &lu2_name='8' and &survey_area_size_name>1 and
&survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='7';
end;

/* Rule 8 */
if (upcase(substr(&lc1_name,1,1))='C' and 
(upcase(&lu1_name)='U111' or upcase(&lu1_name)='U112' or upcase(&lu1_name)='U113') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='3'; 
	condition_fao_class='8_1';
end;

/* Rule 9 */
if (upcase(substr(&lc1_name,1,1))='C' and 
(upcase(&lu2_name)='U111' or upcase(&lu2_name)='U112' or upcase(&lu2_name)='U113') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='3'; 
	condition_fao_class='8_2';
end;

/* Rule 10 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(substr(&lc2_name,1,1))='B' and
(upcase(substr(&lu1_name,1,2))='U4' or upcase(&lu1_name)='U120') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;	
	&fao_class_name='3'; 
	condition_fao_class='8_3';
end;

/* Rule 11 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318'
or upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350'
or upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='9_1';
end;

/* Rule 12 */
if (upcase(substr(&lc1_name,1,1))='C' and 
(upcase(&lu1_name)='U140' or upcase(&lu1_name)='U150' or upcase(substr(&lu1_name,1,2))='U4') and upcase(&lu2_name)='8' and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='9_2';
end;

/* Rule 13 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U350' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U120') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='9_3';
end;

/* Rule 14 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U318' and upcase(&lu2_name)='8' and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='9_4';
end;

/* Rule 15 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or 
upcase(&lu2_name)='U318' or upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or 
upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name=1 and &fao_class_name='') then do;
	&fao_class_name='2'; 
	condition_fao_class='10_1';
end;

/* Rule 16 */
if (upcase(substr(&lc1_name,1,1))='C' and 
(upcase(&lu1_name)='U140' or upcase(&lu1_name)='U150' or upcase(substr(&lu1_name,1,2))='U4') and
upcase(&lu2_name)='8' and &survey_area_size_name>1 and &survey_tree_height_maturity_name=1 and &fao_class_name='') then do;
	&fao_class_name='2'; 
	condition_fao_class='10_2';
end;

/* Rule 17 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U350' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U120') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name=1 and &fao_class_name='') then do;
	&fao_class_name='2'; 
	condition_fao_class='10_3';
end;

/* Rule 18 */
if (
(upcase(substr(&lc1_name,1,1))='D' or upcase(&lc1_name)='E10') and
upcase(&lu1_name)='U120' and 
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or 
upcase(&lu2_name)='U318' or upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or 
upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='2'; 
	condition_fao_class='10_4';
end;

/* Rule 19 */
if (
(upcase(substr(&lc1_name,1,1))='D' or upcase(&lc1_name)='E10') and
(upcase(&lu1_name)='U140' or upcase(&lu1_name)='U150' or upcase(substr(&lu1_name,1,2))='U4') and
upcase(&lu2_name)='8' and &survey_area_size_name>1 and 
&survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='2'; 
	condition_fao_class='10_5';
end;

/* Rule 20 */
if (
(upcase(substr(&lc1_name,1,1))='D' or upcase(&lc1_name)='E10') and
upcase(&lu1_name)='U350' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U120') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='2'; 
	condition_fao_class='10_6';
end;

/* Rule 21 */
if (upcase(&lc1_name)='D10' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318' or
upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or
upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and 
&survey_area_size_name=1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='11_1';
end;

/* Rule 22 */
if (upcase(&lc1_name)='D10' and upcase(substr(&lu1_name,1,2))='U4' and upcase(&lu2_name)='8' and 
&survey_area_size_name=1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='11_2';
end;

/* Rule 23 */
if (upcase(&lc1_name)='E10' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318' or
upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or
upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and 
&survey_area_size_name=1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='11_3';
end;

/* Rule 24 */
if (upcase(&lc1_name)='E10' and upcase(substr(&lu1_name,1,2))='U4' and upcase(&lu2_name)='8' and 
&survey_area_size_name=1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='11_4';
end;

/* Rule 26 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318' or
upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or
upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and 
&survey_area_size_name=1 and &fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='11_5';
end;

/* Rule 27 */
if (upcase(substr(&lc1_name,1,1))='C' and 
(upcase(substr(&lu1_name,1,2))='U2' or upcase(substr(&lu1_name,1,3))='U31' or upcase(substr(&lu1_name,1,3))='U32'
or upcase(substr(&lu1_name,1,3))='U34' or upcase(substr(&lu1_name,1,3))='U36' or upcase(&lu1_name)='U370') and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
	&fao_class_name='3'; 
	condition_fao_class='12_1';
end;

/* Rule 28 */
if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U350' and 
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and
(upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362')
and &fao_class_name='') then do;
	&fao_class_name='3'; 
	condition_fao_class='12_2';
end;

/* Rule 29 */
if (upcase(&lu1_name)='U120' and 
(&survey_lc_lu_special_remark_name=3 or &survey_lc_lu_special_remark_name=4 or &survey_lc_lu_special_remark_name=5) and
&fao_class_name='') then do;
	&fao_class_name='1'; 
	condition_fao_class='13';
end;

/* Rule 30 */
if (&fao_class_name='') then &fao_class_name='0';

run;

%mend AddVariable;
