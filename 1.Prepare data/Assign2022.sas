%web_drop_table(WORK.IMPORT);
FILENAME REFFILE 'D:\Google Drive\LUCAS 2025\1.FAO_LUE_LUD_settlement/survey22_noNA.csv';
PROC IMPORT DATAFILE=REFFILE
        DBMS=CSV
        OUT=WORK.Survey2022;
        GETNAMES=YES;
RUN;
/*
PROC CONTENTS DATA=WORK.Survey2022;
RUN;
*/
%web_open_table(WORK.Survey2022);

%macro AddVariable(in_ds_name,
                                        out_ds_name,
                                        fao_class_name,
                                        lc1_name,
                                        lu1_name,
                                        lc2_name,
                                        lu2_name,
                                        lc1_species_name,
                                        survey_area_size_name,
                                        survey_tree_height_maturity_name,
                                        survey_feature_width_name,
                                        survey_lc_lu_special_remark_name);

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

if (upcase(substr(&lc1_name,1,1))='G' or upcase(substr(&lc1_name,1,1))='H') then do;
        &fao_class_name='0';
        condition_fao_class='1';
end;

if (upcase(&lc1_name)='A22' and upcase(&lu1_name)='U312' and upcase(&lu2_name)='U120' and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='2';
end;

if (upcase(&lc1_name)='A30' and
(upcase(substr(&lc2_name,1,1))='C' or upcase(substr(&lc2_name,1,1))='D' or upcase(substr(&lc2_name,1,1))='E' or
upcase(substr(&lc2_name,1,1))='F') and
upcase(&lu1_name)='U319' and upcase(&lu2_name)='U120' and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='3';
end;

if (upcase(substr(&lc1_name,1,2))='B7' and
(upcase(&lu1_name)='U111' or upcase(&lu1_name)='U112' or upcase(&lu1_name)='U113' or upcase(substr(&lu1_name,1,2))='U4') and
(upcase(&lc1_species_name) ne 'B75E' and upcase(&lc1_species_name) ne 'B75P') and
&survey_area_size_name>1 and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='4';
end;

if (upcase(&lc1_name)='B81' and
(upcase(&lu1_name)='U111' or upcase(&lu1_name)='U112' or upcase(&lu1_name)='U113' or upcase(substr(&lu1_name,1,2))='U4') and
&survey_area_size_name>1 and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='5';
end;

if (upcase(&lc1_species_name)='B83F' and &survey_area_size_name>1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='6';
end;

if (upcase(substr(&lc1_name,1,1))='C' and &lc2_name='8' and upcase(&lu1_name)='U111' and &lu2_name='8' and &survey_area_size_name>1 and
&survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='7';
end;

if (upcase(substr(&lc1_name,1,1))='C' and
(upcase(&lu1_name)='U111' or upcase(&lu1_name)='U112' or upcase(&lu1_name)='U113') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='8_1';
end;

if (upcase(substr(&lc1_name,1,1))='C' and
(upcase(&lu2_name)='U111' or upcase(&lu2_name)='U112' or upcase(&lu2_name)='U113') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='8_2';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(substr(&lc2_name,1,1))='B' and
(upcase(substr(&lu1_name,1,2))='U4' or upcase(&lu1_name)='U120') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='8_3';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318'
or upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350'
or upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='9_1';
end;

if (upcase(substr(&lc1_name,1,1))='C' and
(upcase(&lu1_name)='U140' or upcase(&lu1_name)='U150' or upcase(substr(&lu1_name,1,2))='U4') and upcase(&lu2_name)='8' and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='9_2';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U350' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U120') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='9_3';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U318' and upcase(&lu2_name)='8' and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='9_4';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or
upcase(&lu2_name)='U318' or upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or
upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name=1 and &fao_class_name='') then do;
        &fao_class_name='2';
        condition_fao_class='10_1';
end;

if (upcase(substr(&lc1_name,1,1))='C' and
(upcase(&lu1_name)='U140' or upcase(&lu1_name)='U150' or upcase(substr(&lu1_name,1,2))='U4') and
upcase(&lu2_name)='8' and &survey_area_size_name>1 and &survey_tree_height_maturity_name=1 and &fao_class_name='') then do;
        &fao_class_name='2';
        condition_fao_class='10_2';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U350' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U120') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name=1 and &fao_class_name='') then do;
        &fao_class_name='2';
        condition_fao_class='10_3';
end;

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

if (
(upcase(substr(&lc1_name,1,1))='D' or upcase(&lc1_name)='E10') and
(upcase(&lu1_name)='U140' or upcase(&lu1_name)='U150' or upcase(substr(&lu1_name,1,2))='U4') and
upcase(&lu2_name)='8' and &survey_area_size_name>1 and
&survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='2';
        condition_fao_class='10_5';
end;

if (
(upcase(substr(&lc1_name,1,1))='D' or upcase(&lc1_name)='E10') and
upcase(&lu1_name)='U350' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U120') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='2';
        condition_fao_class='10_6';
end;

if (upcase(&lc1_name)='D10' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318' or
upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or
upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and
&survey_area_size_name=1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='11_1';
end;

if (upcase(&lc1_name)='D10' and upcase(substr(&lu1_name,1,2))='U4' and upcase(&lu2_name)='8' and
&survey_area_size_name=1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='11_2';
end;

if (upcase(&lc1_name)='E10' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318' or
upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or
upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and
&survey_area_size_name=1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='11_3';
end;

if (upcase(&lc1_name)='E10' and upcase(substr(&lu1_name,1,2))='U4' and upcase(&lu2_name)='8' and
&survey_area_size_name=1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='11_4';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U120' and
(upcase(&lu2_name)='8' or upcase(&lu2_name)='U140' or upcase(&lu2_name)='U150' or upcase(&lu2_name)='U318' or
upcase(&lu2_name)='U321' or upcase(&lu2_name)='U322' or upcase(&lu2_name)='U350' or upcase(&lu2_name)='U361' or
upcase(&lu2_name)='U362' or upcase(&lu2_name)='U370') and
&survey_area_size_name=1 and &fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='11_5';
end;

if (upcase(substr(&lc1_name,1,1))='C' and
(upcase(substr(&lu1_name,1,2))='U2' or upcase(substr(&lu1_name,1,3))='U31' or upcase(substr(&lu1_name,1,3))='U32'
or upcase(substr(&lu1_name,1,3))='U34' or upcase(substr(&lu1_name,1,3))='U36' or upcase(&lu1_name)='U370') and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='12_1';
end;

if (upcase(substr(&lc1_name,1,1))='C' and upcase(&lu1_name)='U350' and
&survey_area_size_name>1 and &survey_tree_height_maturity_name>1 and &survey_feature_width_name>1 and
(upcase(&lu2_name)='U361' or upcase(&lu2_name)='U362')
and &fao_class_name='') then do;
        &fao_class_name='3';
        condition_fao_class='12_2';
end;

if (upcase(&lu1_name)='U120' and
(&survey_lc_lu_special_remark_name=3 or &survey_lc_lu_special_remark_name=4 or &survey_lc_lu_special_remark_name=5) and
&fao_class_name='') then do;
        &fao_class_name='1';
        condition_fao_class='13';
end;

if (&fao_class_name='') then &fao_class_name='0';

run;

%mend AddVariable;


%AddVariable(
                WORK.Survey2022,
                WORK.Survey2022_with_FAO,
                fao_class_name,
                SURVEY_LC1,
                SURVEY_LU1,
                SURVEY_LC2,
                SURVEY_LU2,
                SURVEY_LC1_SPEC,
                SURVEY_PARCEL_AREA_HA,
                SURVEY_TREE_HEIGHT_MATURITY,
                SURVEY_FEATURE_WIDTH,
                SURVEY_LC_LU_SPECIAL_REMARK
);


PROC FREQ DATA=WORK.Survey2022_with_FAO;
 TABLES fao_class_name;
 TABLES condition_fao_class;
 TABLES condition_fao_class * fao_class_name;
RUN;
  /*
 TABLES SURVEY_LC1;
 TABLES SURVEY_LU1;
 TABLES SURVEY_LC2;
 TABLES SURVEY_LU2;
 TABLES SURVEY_LC1_SPEC;
 TABLES SURVEY_PARCEL_AREA_HA;
 TABLES SURVEY_TREE_HEIGHT_MATURITY;
 TABLES SURVEY_FEATURE_WIDTH;
 TABLES SURVEY_LC_LU_SPECIAL_REMARK;

RUN;
 */
PROC EXPORT DATA=WORK.Survey2022_with_FAO
    OUTFILE="D:\Google Drive\LUCAS 2025\1.FAO_LUE_LUD_settlement/SURVEY2022_WITH_FAO.csv"
    DBMS=CSV
    REPLACE;
    DELIMITER=',';
    PUTNAMES=YES;
RUN;
