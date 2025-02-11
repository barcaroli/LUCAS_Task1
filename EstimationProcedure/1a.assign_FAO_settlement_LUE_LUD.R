#----------------------------------------------------------
# Script to assign FAO forest, settlement, LUE and LUD
#----------------------------------------------------------
# called by 1.prepare_LUCAS_input.R
# Output: FAO2022.csv


s2022$fao_class_name <- NA
s2022$fao_condition <- NA

# 1
# if (substr(s2022$SURVEY_LC1, 1, 1)) == 'G' | substr(s2022$SURVEY_LC1, 1, 1)) == 'H') {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'G' | substr(s2022$SURVEY_LC1, 1, 1) == 'H'] <- "0"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'G' | substr(s2022$SURVEY_LC1, 1, 1) == 'H'] <- "1"
# }

# 2
# if (s2022$SURVEY_LC1 == 'A22' & 
#     s2022$SURVEY_LU1 == 'U312' & 
#     s2022$SURVEY_LU2 == 'U120' & 
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LC1 == 'A22' & 
                     s2022$SURVEY_LU1 == 'U312' & 
                     s2022$SURVEY_LU2 == 'U120' & 
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LC1 == 'A22' & 
                    s2022$SURVEY_LU1 == 'U312' & 
                    s2022$SURVEY_LU2 == 'U120' & 
                    is.na(s2022$fao_condition)] <- "2"
# }

# 3
# if (s2022$SURVEY_LC1 == 'A30' &
#     substr(s2022$SURVEY_LC2, 1, 1) %in% c('C', 'D', 'E', 'F') &
#     s2022$SURVEY_LU1 == 'U319' &
#     s2022$SURVEY_LU2 == 'U120' &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LC1 == 'A30' &
                     substr(s2022$SURVEY_LC2, 1, 1) %in% c('C', 'D', 'E', 'F') &
                     s2022$SURVEY_LU1 == 'U319' &
                     s2022$SURVEY_LU2 == 'U120' &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LC1 == 'A30' &
                    substr(s2022$SURVEY_LC2, 1, 1) %in% c('C', 'D', 'E', 'F') &
                    s2022$SURVEY_LU1 == 'U319' &
                    s2022$SURVEY_LU2 == 'U120' &
                    is.na(s2022$fao_condition)] <- "3"
# }

# 4
# if (substr(s2022$SURVEY_LC1, 1, 2) == 'B7' &
#     (s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
#     !s2022$SURVEY_LC1_SPEC %in% c('B75E', 'B75P') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     is.na(s2022$fao_class_name)) {
  # MODIFIED ADDING BX2
  s2022$fao_class_name[(substr(s2022$SURVEY_LC1, 1, 2) == 'B7' | s2022$SURVEY_LC1 == 'BX2') &
                     (s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                     !s2022$SURVEY_LC1_SPEC %in% c('B75E', 'B75P') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[(substr(s2022$SURVEY_LC1, 1, 2) == 'B7' | s2022$SURVEY_LC1 == 'BX2')  &
                    (s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                    !s2022$SURVEY_LC1_SPEC %in% c('B75E', 'B75P') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    is.na(s2022$fao_condition)] <- "4"
# }

# if (s2022$SURVEY_LC1 == 'B81' &
#     (s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     is.na(s2022$fao_class_name)) {
  # MODIFIED ADDING BX2
  s2022$fao_class_name[(s2022$SURVEY_LC1 == 'B81' | s2022$SURVEY_LC1 == 'BX2') &
                     (s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[(s2022$SURVEY_LC1 == 'B81' | s2022$SURVEY_LC1 == 'BX2') &
                    (s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    is.na(s2022$fao_condition)] <- "5"
# }
  
    
# 6
# if (s2022$SURVEY_LC1_SPEC == 'B83F' &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     is.na(s2022$fao_class_name)) {
  # MODIFIED ADDING BX2
  s2022$fao_class_name[(s2022$SURVEY_LC1_SPEC == 'B83F' | s2022$SURVEY_LC1 == 'BX2') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[(s2022$SURVEY_LC1_SPEC == 'B83F' | s2022$SURVEY_LC1 == 'BX2') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    is.na(s2022$fao_condition)] <- "6"
# }

# 7
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LC2 == '8' &
#     s2022$SURVEY_LU1 == 'U111' &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LC2 == '8' &
                     s2022$SURVEY_LU1 == 'U111' &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LC2 == '8' &
                    s2022$SURVEY_LU1 == 'U111' &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "7"
# }

# 8_1
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 %in% c('U111', 'U112', 'U113') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "8_1"
# }

# 8_2
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU2 %in% c('U111', 'U112', 'U113') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU2 %in% c('U111', 'U112', 'U113') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU2 %in% c('U111', 'U112', 'U113') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "8_2"
# }

# 8_3
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     substr(s2022$SURVEY_LC2, 1, 1) == 'B' &
#     (substr(s2022$SURVEY_LU1, 1, 2) == 'U4' | s2022$SURVEY_LU1 == 'U120') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     substr(s2022$SURVEY_LC2, 1, 1) == 'B' &
                     (substr(s2022$SURVEY_LU1, 1, 2) == 'U4' | s2022$SURVEY_LU1 == 'U120') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    substr(s2022$SURVEY_LC2, 1, 1) == 'B' &
                    (substr(s2022$SURVEY_LU1, 1, 2) == 'U4' | s2022$SURVEY_LU1 == 'U120') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "8_3"
# }

# 9_1
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "9_1"
# }

# 9_2
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "9_2"
# }

# 9_3
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U350' &
#     s2022$SURVEY_LU2 %in% c('8', 'U120') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U350' &
                     s2022$SURVEY_LU2 %in% c('8', 'U120') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U350' &
                    s2022$SURVEY_LU2 %in% c('8', 'U120') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "9_3"
# }

# 9_4
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U318' &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U318' &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U318' &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "9_4"
# }

# 10_1
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
                     is.na(s2022$fao_class_name)] <- "2"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
                    is.na(s2022$fao_condition)] <- "10_1"
# }

# 10_2
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
                     is.na(s2022$fao_class_name)] <- "2"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
                    is.na(s2022$fao_condition)] <- "10_2"
# }

# 10_3
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U350' &
#     s2022$SURVEY_LU2 %in% c('8', 'U120') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U350' &
                     s2022$SURVEY_LU2 %in% c('8', 'U120') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
                     is.na(s2022$fao_class_name)] <- "2"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U350' &
                    s2022$SURVEY_LU2 %in% c('8', 'U120') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY == 1 &
                    is.na(s2022$fao_condition)] <- "10_3"
# }

# 10_4
# if ((substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
#     s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[(substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
                     s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "2"
  s2022$fao_condition[(substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
                    s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "10_4"
# }

# 10_5
# if ((substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
#     (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[(substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
                     (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "2"
  s2022$fao_condition[(substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10' ) &
                    (s2022$SURVEY_LU1 %in% c('U140', 'U150') | substr(s2022$SURVEY_LU1, 1, 2) == 'U4') &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "10_5"
# }

# 10_6
# if ((substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
#     s2022$SURVEY_LU1 == 'U350' &
#     s2022$SURVEY_LU2 %in% c('8', 'U120') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[(substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
                     s2022$SURVEY_LU1 == 'U350' &
                     s2022$SURVEY_LU2 %in% c('8', 'U120') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "2"
  s2022$fao_condition[(substr(s2022$SURVEY_LC1, 1, 1) == 'D' | s2022$SURVEY_LC1 == 'E10') &
                    s2022$SURVEY_LU1 == 'U350' &
                    s2022$SURVEY_LU2 %in% c('8', 'U120') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "10_6"
# }

# 11_1
# if (s2022$SURVEY_LC1 == 'D10' &
#     s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
#     s2022$SURVEY_PARCEL_AREA_HA == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LC1 == 'D10' &
                     s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA == 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LC1 == 'D10' &
                    s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA == 1 &
                    is.na(s2022$fao_condition)] <- "11_1"
# }

# 11_2
# if (s2022$SURVEY_LC1 == 'D10' &
#     substr(s2022$SURVEY_LU1, 1, 2) == 'U4' &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LC1 == 'D10' &
                     substr(s2022$SURVEY_LU1, 1, 2) == 'U4' &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA == 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LC1 == 'D10' &
                    substr(s2022$SURVEY_LU1, 1, 2) == 'U4' &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA == 1 &
                    is.na(s2022$fao_condition)] <- "11_2"
# }

# 11_3
# if (s2022$SURVEY_LC1 == 'E10' &
#     s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
#     s2022$SURVEY_PARCEL_AREA_HA == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LC1 == 'E10' &
                     s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA == 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LC1 == 'E10' &
                    s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA == 1 &
                    is.na(s2022$fao_condition)] <- "11_3"
# }

# 11_4
# if (s2022$SURVEY_LC1 == 'E10' &
#     substr(s2022$SURVEY_LU1, 1, 2) == 'U4' &
#     s2022$SURVEY_LU2 == '8' &
#     s2022$SURVEY_PARCEL_AREA_HA == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LC1 == 'E10' &
                     substr(s2022$SURVEY_LU1, 1, 2) == 'U4' &
                     s2022$SURVEY_LU2 == '8' &
                     s2022$SURVEY_PARCEL_AREA_HA == 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LC1 == 'E10' &
                    substr(s2022$SURVEY_LU1, 1, 2) == 'U4' &
                    s2022$SURVEY_LU2 == '8' &
                    s2022$SURVEY_PARCEL_AREA_HA == 1 &
                    is.na(s2022$fao_condition)] <- "11_4"
# }

# 11_5
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
#     s2022$SURVEY_PARCEL_AREA_HA == 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA == 1 &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LU2 %in% c('8', 'U140', 'U150', 'U318', 'U321', 'U322', 'U350', 'U361', 'U362', 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA == 1 &
                    is.na(s2022$fao_condition)] <- "11_5"
# }

# 12_1
# if (substr(s2022$SURVEY_LC1,1,1) == "C" & 
  # (substr(s2022$SURVEY_LU1,1,2) == 'U2' |
  #     substr(s2022$SURVEY_LU1,1,3) %in% c('U31', 'U32', 'U34', 'U36') |
  #     s2022$SURVEY_LU1 == 'U370') &
  #   s2022$SURVEY_PARCEL_AREA_HA > 1 &
  #   s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
  #   s2022$SURVEY_FEATURE_WIDTH > 1 &
  #   is.na(s2022$fao_class_name)] {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1,1,1) == "C" & 
                     (substr(s2022$SURVEY_LU1,1,2) == 'U2' |
                     substr(s2022$SURVEY_LU1,1,3) %in% c('U31', 'U32', 'U34', 'U36') |
                     s2022$SURVEY_LU1 == 'U370') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[substr(s2022$SURVEY_LC1,1,1) == "C" & 
                    (substr(s2022$SURVEY_LU1,1,2) == 'U2' |
                       substr(s2022$SURVEY_LU1,1,3) %in% c('U31', 'U32', 'U34', 'U36') |
                       s2022$SURVEY_LU1 == 'U370') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "12_1"
# }

# 12_2
# if (substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
#     s2022$SURVEY_LU1 == 'U350' &
#     s2022$SURVEY_LU2 %in% c('U361', 'U362') &
#     s2022$SURVEY_PARCEL_AREA_HA > 1 &
#     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
#     s2022$SURVEY_FEATURE_WIDTH > 1 &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                     s2022$SURVEY_LU1 == 'U350' &
                     s2022$SURVEY_LU2 %in% c('U361', 'U362') &
                     s2022$SURVEY_PARCEL_AREA_HA > 1 &
                     s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                     s2022$SURVEY_FEATURE_WIDTH > 1 &
                     is.na(s2022$fao_class_name)] <- "3"
  s2022$fao_condition[substr(s2022$SURVEY_LC1, 1, 1) == 'C' &
                    s2022$SURVEY_LU1 == 'U350' &
                    s2022$SURVEY_LU2 %in% c('U361', 'U362') &
                    s2022$SURVEY_PARCEL_AREA_HA > 1 &
                    s2022$SURVEY_TREE_HEIGHT_MATURITY > 1 &
                    s2022$SURVEY_FEATURE_WIDTH > 1 &
                    is.na(s2022$fao_condition)] <- "12_2"
# }

# 13
# if (s2022$SURVEY_LU1 == 'U120' &
#     s2022$SURVEY_LC_LU_SPECIAL_REMARK %in% c(3, 4, 5) &
#     is.na(s2022$fao_class_name)) {
  s2022$fao_class_name[s2022$SURVEY_LU1 == 'U120' &
                     s2022$SURVEY_LC_LU_SPECIAL_REMARK %in% c(3, 4, 5) &
                     is.na(s2022$fao_class_name)] <- "1"
  s2022$fao_condition[s2022$SURVEY_LU1 == 'U120' &
                    s2022$SURVEY_LC_LU_SPECIAL_REMARK %in% c(3, 4, 5) &
                    is.na(s2022$fao_condition)] <- "13"
# }

s2022$fao_class_name <- ifelse(is.na(s2022$fao_class_name),0,s2022$fao_class_name)

prop.table(table(s2022$fao_class_name))
prop.table(table(s2022$fao_condition,useNA="ifany"))
table(s2022$fao_condition,s2022$fao_class_name,useNA="ifany")

####### Settlement

s2022$settlement <- ifelse(s2022$SURVEY_LC1 %in% c("A10", "A11", "A12", "A13", "A20", "A21", "A22", "A30") |
                             s2022$SURVEY_LU1 %in% c("U210", "U220", "U221", "U222", "U223", "U224", "U225", "U226", "U227", 
                                                     "U228", "U310", "U311", "U312", "U314", "U315", "U316", "U317", "U318", 
                                                     "U319", "U320", "U321", "U322", "U330", "U340", "U341", "U342", "U350", 
                                                     "U360", "U362", "U370"), 1, 0 )  
table(s2022$settlement)

####### LUD
# o	LUD, equals to 1 if: 
#     	LU=U14*, U21*, U22*, U32*, U33*, U31* (where * stands for any other digit)
s2022$lud <- ifelse(substr(s2022$SURVEY_LU1,1,3) %in% c("U14", "U21", "U22", "U32", "U33", "U31"), 1, 0 )  
table(s2022$lud,useNA="ifany")

####### LUE
# o	LUE equals to 1 if:
#     	LU= U34*, U35*, U36*, U37*
s2022$lue <- ifelse(substr(s2022$SURVEY_LU1,1,3) %in% c("U34", "U35", "U36", "U37"), 1, 0 )  
table(s2022$lue,useNA="ifany")


### WRITE

df <- s2022[,c("POINT_ID","SURVEY_OBS_TYPE","fao_class_name","fao_condition")]
df$ones <- 1
xtabs(df,formula=~fao_condition+ones,addNA=T)
100*addmargins(prop.table(xtabs(df,formula=~fao_class_name,addNA=T)))


write.table(df,"FAO2022.csv",sep=",",quote=F,row.names = F)




