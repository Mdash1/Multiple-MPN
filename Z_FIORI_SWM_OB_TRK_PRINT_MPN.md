FUNCTION z_fiori_swm_ob_trk_print_mpn.
*"----------------------------------------------------------------------
*"*"Local Interface: *" IMPORTING *" VALUE(I_LGNUM) TYPE /SCWM/LGNUM *"
VALUE(I_REPORT_NO) TYPE YREPORT_NO *" EXPORTING *" VALUE(E_XSTRING) TYPE
TR_XSTRING *" VALUE(E_FILENAME) TYPE /BOFU/FIELD_NAME *"
VALUE(E_MIMETYPE) TYPE W3CONTTYPE *" VALUE(ET_RETURN) TYPE BAPIRET2_T
*"----------------------------------------------------------------------
*----------------------------------------------------------------------- *
CHANGE HISTORY
*-----------------------------------------------------------------------
*SrNo\| Date \| User ID \| Description
*----------------------------------------------------------------------- *
1 \| 02.11.2018 \|KPIT_064 \| Development for TOL changes in SWH for
polyster \* functional Abdul Wasim \* 8027881: Development for TOL
changes in SWH \| \* 2 \| 31.05.2019 \|BRILO_007\| Changes to Update
Storage Bin from \* /SCWM/ORDIM_C table based on value from \* RD2 param
FC: Abdul Wasim CD:8034207
\*-----------------------------------------------------------------------

TYPES : BEGIN OF gty_tu_sr_act, veh_num TYPE /scwm/de_veh_num,
veh_sr_act_num TYPE /scwm/de_sr_act_num, END OF gty_tu_sr_act,

          BEGIN OF gty_ordim_c_temp,
           lgnum       TYPE /scwm/lgnum,
           tanum       TYPE /scwm/tanum,
           tapos       TYPE   /scwm/tapos,
           trart       TYPE /scwm/lvs_trart,
           tostat      TYPE /scwm/de_tostat,
           matid       TYPE /scwm/de_matid,
           stock_itmno TYPE /scwm/de_stock_itmno,
           charg       TYPE /scwm/de_charg,
           meins       TYPE /scwm/de_base_uom,
           vsolm       TYPE /scwm/ltap_vsolm,
           vsola       TYPE /scwm/ltap_vsola,
           idplate     TYPE /scwm/de_idplate_wt,
           vlpla       TYPE /scwm/ltap_vlpla,
           sguid_hu    TYPE /scwm/guid_hu,
           vlenr       TYPE /scwm/ltap_vlenr,
           nlber       TYPE  /scwm/ltap_nlber,
           dguid_hu    TYPE /scwm/guid_hu,
           nlenr       TYPE  /scwm/ltap_nlenr,
           rdoccat     TYPE  /scwm/de_doccat,
           rdocid      TYPE /scwm/de_docid,
           ritmid      TYPE /scwm/de_itmid,
           tcode       TYPE /scwm/table_log_tcode,
           zzexp_srno  TYPE   zexp_serno,
          END OF gty_ordim_c_temp,

          BEGIN OF gty_final,                               "#EC NEEDED
          tu_num  TYPE /scwm/de_tu_num,
          lgnum   TYPE /scwm/lgnum,
          tanum   TYPE /scwm/tanum,
          veh_num TYPE /scwm/de_veh_num,
          END OF gty_final,

          BEGIN OF gty_outtab,
          lgnum TYPE /scwm/lgnum,
          who   TYPE /scwm/de_who,
          wave  TYPE /scwm/de_wave,
          END OF gty_outtab,

          BEGIN OF gty_huhdr_t,
          guid_hu TYPE /scwm/guid_hu ,
          huident TYPE /scwm/de_huident ,
          END OF gty_huhdr_t,

          BEGIN OF gty_huhdr,
          guid_hu        TYPE /scwm/guid_hu,
          huident        TYPE /scwm/de_huident,
          checkbox(1),
          no_of_child(3) TYPE n,
          END OF gty_huhdr,

          BEGIN OF gty_gmhuhdr,
          guid_hu        TYPE /scwm/guid_hu,
          guid_parent    TYPE /lime/guid_parent,
          huident        TYPE /scwm/de_huident,
          checkbox(1),
          no_of_child(3) TYPE n,
          END OF gty_gmhuhdr,

          BEGIN OF gty_docid2,                              "#EC NEEDED
          docid TYPE /scwm/de_docid,
          END OF gty_docid2,

          BEGIN OF gty_dlvno,
          docid     TYPE /scdl/dl_docid,
          refdoccat TYPE /scdl/dl_refdoccat,
          refdocno  TYPE /scdl/dl_refdocno,
          END OF gty_dlvno,

          BEGIN OF gty_delv_no,
          deliveryno TYPE vbeln_vl,
          END OF gty_delv_no,

-   Initial Form

           BEGIN OF gty_delv_st,
           deliveryno       TYPE vbeln_vl,
           c_rpno           TYPE char20,
           s_rpno           TYPE char20,
           rpexpdat         TYPE sydatum,
           shnumber         TYPE char10,
           lr_no            TYPE char10,
           report_no        TYPE char10,
           name1            TYPE name1,
           kbetr            TYPE string,
           umren            TYPE umren,
           name2            TYPE name2,
           name3            TYPE name3_gp,
           contno           TYPE char13,
           driver           TYPE char35,
           licno            TYPE char20,
           permit           TYPE char20,
           transptrcd       TYPE char10,
           zrpno            TYPE char15,
           shpdat           TYPE sydatum,
           name4            TYPE name4_gp,
           kunnr            TYPE kunnr,
           city1            TYPE char40,
           bezei            TYPE bezei,
           price            TYPE p LENGTH 11 DECIMALS 2 ,
           remark           TYPE string,
           waerk            TYPE waerk ,
           END OF gty_delv_st,

           BEGIN OF gty_log_st,
           deliveryno TYPE vbeln_vl,
           message    TYPE char30,
           END OF gty_log_st,

           BEGIN OF gty_marm ,
           matid TYPE /sapapo/matid ,
           meinh TYPE /sapapo/lrmei ,
           umrez TYPE /sapapo/umrez ,
           umren TYPE /sapapo/umren ,
           END OF gty_marm,

           BEGIN OF gty_trans,
           docid          TYPE /scdl/dl_docid,
           transmeans_id  TYPE /scdl/dl_transmeans_id,
           /scwm/stop_seq TYPE   /scmb/de_seq_num,
           END OF gty_trans,

           BEGIN OF gty_but020,                              "#EC NEEDED
           partner    TYPE bu_partner,
           addrnumber TYPE ad_addrnum,
           END OF gty_but020,

           BEGIN OF gty_lnumt,                               "#EC NEEDED
           spras   TYPE spras,
           lgnum  TYPE /scwm/lgnum,
           lnumt   TYPE /scwm/de_desc40,
           END OF gty_lnumt,

           BEGIN OF  gty_plant,                              "#EC NEEDED
           entitled  TYPE  /scwm/de_entitled_dflt,
           lgnum     TYPE  /scwm/lgnum ,
           END OF  gty_plant,

           BEGIN OF lty_ordim_tmp,
           count       TYPE i,
           lgnum       TYPE /scwm/lgnum,
           tanum       TYPE /scwm/tanum,
           trart       TYPE /scwm/lvs_trart,
           tostat      TYPE  /scwm/de_tostat,
           matid       TYPE  /scwm/de_matid,
           stock_itmno   TYPE /scwm/de_stock_itmno,
           charg       TYPE  /scwm/de_charg,
           meins       TYPE  /scwm/de_base_uom,
           vsolm       TYPE  /scwm/ltap_vsolm,
           vsola       TYPE  /scwm/ltap_vsola,
           idplate       TYPE /scwm/de_idplate_wt,
           sguid_hu    TYPE  /scwm/guid_hu,
           vlpla       TYPE /scwm/ltap_vlpla,
           vlenr       TYPE /scwm/ltap_vlenr,
           nlber       TYPE  /scwm/ltap_nlber,
           dguid_hu    TYPE  /scwm/guid_hu,
           nlenr       TYPE  /scwm/ltap_nlenr,
           rdoccat     TYPE  /scwm/de_doccat,
           rdocid      TYPE  /scwm/de_docid,
           ritmid      TYPE  /scwm/de_itmid,
           tcode       TYPE  /scwm/table_log_tcode,
           zzexp_srno  TYPE  zexp_serno,
           END OF lty_ordim_tmp,

    ***SOC BRILO_007 Dt 31/05/2019 BEGIN OF gty_ordim_c\_sb, lgnum TYPE
    /scwm/lgnum, procty TYPE /scwm/de_procty, vlpla TYPE
    /scwm/ltap_vlpla, vlenr TYPE /scwm/ltap_vlenr, nlpla TYPE
    /scwm/ltap_nlpla, END OF gty_ordim_c\_sb, ***EOC BRILO_007 Dt
    31/05/2019 BEGIN OF gty_ordim_o, lgnum TYPE /scwm/lgnum, tanum TYPE
    /scwm/tanum, trart TYPE /scwm/lvs_trart, tostat TYPE
    /scwm/de_tostat, matid TYPE /scwm/de_matid, stock_itmno TYPE
    /scwm/de_stock_itmno, charg TYPE /scwm/de_charg, meins TYPE
    /scwm/de_base_uom, vsolm TYPE /scwm/ltap_vsolm, vsola TYPE
    /scwm/ltap_vsola, idplate TYPE /scwm/de_idplate_wt, vlpla TYPE
    /scwm/ltap_vlpla, sguid_hu TYPE /scwm/guid_hu, vlenr TYPE
    /scwm/ltap_vlenr, nlber TYPE /scwm/ltap_nlber, dguid_hu TYPE
    /scwm/guid_hu, nlenr TYPE /scwm/ltap_nlenr, rdoccat TYPE
    /scwm/de_doccat, rdocid TYPE /scwm/de_docid, ritmid TYPE
    /scwm/de_itmid, tcode TYPE /scwm/table_log_tcode, zzexp_srno TYPE
    zexp_serno, END OF gty_ordim_o.

TYPES: BEGIN OF lty_veh_num, veh_num TYPE /scwm/de_veh_num, lic_plate
TYPE /scwm/de_lic_plate, created_at TYPE tzntstmps, END OF lty_veh_num.

DATA: gw_lgnum TYPE /scwm/lgnum , "#EC NEEDED gw_veh_num TYPE
/scwm/de_veh_num ,"#EC NEEDED gw_lic_plate TYPE /scwm/de_lic_plate ,
"#EC NEEDED gw_veh_sr_act TYPE /scwm/de_sr_act_num ,"#EC NEEDED
gw_tu_sr_act TYPE /scwm/de_tu_sr_act_num, "#EC NEEDED gt_sr_act TYPE
TABLE OF gty_tu_sr_act ,"#EC NEEDED gw_sr_act TYPE gty_tu_sr_act, "#EC
NEEDED gw_index TYPE i,"#EC NEEDED gw_c TYPE c, gw_ok_code TYPE
sy-ucomm, "#EC NEEDED gt_fcat TYPE slis_t\_fieldcat_alv,"#EC NEEDED
gw_fcat LIKE LINE OF gt_fcat, "#EC NEEDED gt_ordim_o TYPE TABLE OF
gty_ordim_o,"#EC NEEDED gt_ordim_o\_t TYPE TABLE OF gty_ordim_o, "#EC
NEEDED gt_ordim_c\_t TYPE TABLE OF gty_ordim_o, gt_ordim_o2 TYPE TABLE
OF gty_ordim_o,"#EC NEEDED gw_ordim_o TYPE gty_ordim_o, "#EC NEEDED
gw_ordim_o2 TYPE gty_ordim_o,"#EC NEEDED gt_ordim_c TYPE TABLE OF
gty_ordim_o, "#EC NEEDED gt_ordim_c2 TYPE TABLE OF gty_ordim_o,"#EC
NEEDED gw_ordim_c TYPE gty_ordim_o, "#EC NEEDED gw_nlber TYPE
/scwm/ltap_nlber,"#EC NEEDED gt_ordim_c\_temp TYPE TABLE OF
gty_ordim_c\_temp, "#EC NEEDED gw_ordim_c\_temp TYPE gty_ordim_c\_temp,
gt_huident TYPE /scwm/tt_huident,"#EC NEEDED gt\_/scwm/vehicle TYPE
TABLE OF lty_veh_num , "#EC NEEDED gw\_/scwm/vehicle TYPE lty_veh_num ,
gw_huident TYPE /scwm/s_huident,"#EC NEEDED gt_who TYPE TABLE OF
gty_outtab, "#EC NEEDED gw_who TYPE gty_outtab,"#EC NEEDED gw_who1 TYPE
/scwm/who, "#EC NEEDED gt_whohu TYPE /scwm/tt_whohu_int,"#EC NEEDED
gw_whohu TYPE /scwm/s_whohu, "#EC NEEDED gw_tu_num TYPE
/scwm/de_tu_num,"#EC NEEDED gt_docid TYPE /scwm/tt_rdocid, "#EC NEEDEd
gt_docid_temp TYPE /scwm/tt_rdocid,"#EC NEEDEd gw_count TYPE i, "#EC
NEEDED gw_fm_name TYPE rs38l_fnam,"#EC NEEDED gt_huhdr_mult TYPE
/scwm/tt_huhdr_int, "#EC NEEDED gw_huhdr_mult TYPE
/scwm/s_huhdr_int,"#EC NEEDED gw_huhdr_mult1 TYPE /scwm/s_huhdr_int,
"#EC NEEDED gw_output TYPE ssfcompop,"#EC NEEDED gt_docid2 TYPE TABLE OF
gty_docid2, "#EC NEEDED gt_gmhuhdr TYPE TABLE OF gty_gmhuhdr,"#EC NEEDED
gt_gmhuhdr1 TYPE TABLE OF /scwm/gmhuhdr, "#EC NEEDED gt_gmhuhdr2 TYPE
TABLE OF /scwm/gmhuhdr,"#EC NEEDED gw_gmhuhdr2 TYPE /scwm/gmhuhdr, "#EC
NEEDED gw_gmhuhdr1 TYPE /scwm/gmhuhdr,"#EC NEEDED gw_gmhuhdr TYPE
gty_gmhuhdr, "#EC NEEDED gt_huhdr_t TYPE TABLE OF gty_huhdr_t,"#EC
NEEDED gw_huhdr_t TYPE gty_huhdr_t , "#EC NEEDED gt_dlvno TYPE STANDARD
TABLE OF gty_dlvno,"#EC NEEDED gw_dlvno TYPE gty_dlvno, gt_delv_no TYPE
STANDARD TABLE OF gty_delv_no, "#EC NEEDED gw_delv_no TYPE
gty_delv_no,"#EC NEEDED lw_rfcdest TYPE rfcdest, "#EC NEEDED gt_err_log
TYPE STANDARD TABLE OF zlog_boe_dlv_detail_st,"#EC NEEDED gw_err_log
TYPE zlog_boe_dlv_detail_st, "#EC NEEDED gw_boe_flg TYPE c ," "#EC
NEEDED gw_boe_avail TYPE c, gt_fcat1 TYPE slis_t\_fieldcat_alv,"#EC
NEEDED gt_boe TYPE TABLE OF zlog_boe_dlv_detail_st, "#EC NEEDED gw_boe
TYPE zlog_boe_dlv_detail_st,"#EC NEEDED gt_huhdr TYPE TABLE OF
gty_huhdr, "#EC NEEDED gw_huhdr TYPE gty_huhdr,"#EC NEEDED gw_text TYPE
lvc_title, gt_ordim_o\_tt TYPE zewm_ordim_o\_tt, gw_ordim_o\_tt TYPE
zewm_ordim_o\_st.

DATA: gt_log_st TYPE STANDARD TABLE OF gty_log_st, "#EC NEEDED gw_no_wbn
TYPE c,"#EC NEEDED gt_trans TYPE TABLE OF gty_trans, "#EC NEEDED
gw_trans TYPE gty_trans,"#EC NEEDED gw_vlpla TYPE /scwm/ltap_vlpla, "#EC
NEEDED gw_batch TYPE /scwm/de_charg,"#EC NEEDED gt_waveitm_temp TYPE
zewm_waveitm_tt, "#EC NEEDED gw_waveitm_temp TYPE zewm_waveitm_st,"#EC
NEEDED lw_return TYPE bapiret2.

DATA : gw_lnumt TYPE /scwm/de_desc40 , "#EC NEEDED gt_tunit TYPE TABLE
OF /scwm/tunit ,"#EC NEEDED gw_tunit TYPE /scwm/tunit , "#EC NEEDED
gw_db_refdoc TYPE zewm_db_ref_doc_st ,"#EC NEEDED gt_but000 TYPE
zewm_but000_tt, "#EC NEEDED gw_but000 TYPE zewm_but000_st,"#EC NEEDED
gw_but0001 TYPE zewm_but000_st, "#EC NEEDED gw_binsize TYPE i, gt_lines
TYPE TABLE OF tline WITH HEADER LINE, gt_delv_st TYPE zlog_get_detail_tt
," STANDARD TABLE OF GTY_DELV_ST , "#EC NEEDED gw_delv_st TYPE
zlog_get_detail_st ," GTY_DELV_ST , "#EC NEEDED gw_waveitm TYPE
zewm_waveitm_st,"#EC NEEDED gt_waveitm_c TYPE zewm_waveitm_tt, "#EC
NEEDED gt_waveitm TYPE zewm_waveitm_tt,"#EC NEEDED gt_waveitm_tmp TYPE
zewm_waveitm_tt, "#EC NEEDED gt_it_waveitm TYPE zewm_waveitm_tt,"#EC
NEEDED gt_it_waveitm_tmp TYPE zewm_waveitm_tt, "#EC NEEDED
gt_ordim_c\_o2 TYPE TABLE OF gty_ordim_o ,"#EC NEEDED gt_ordim_c\_o TYPE
TABLE OF gty_ordim_o , "#EC NEEDED gw_ordim_c2 TYPE gty_ordim_o ,"#EC
NEEDED gw_ordim_c\_o2 TYPE gty_ordim_o, "#EC NEEDED gw_ordim_c\_o TYPE
gty_ordim_o ,"#EC NEEDED gt_ordim_o\_add TYPE zewm_ordim_o\_tt , "#EC
NEEDED gt_ordim_o\_add_t TYPE zewm_ordim_o\_tt ,"#EC NEEDED
gw_ordim_o\_add TYPE zewm_ordim_o\_st , "#EC NEEDED gt_marm TYPE
STANDARD TABLE OF gty_marm ,"#EC NEEDED gw_marm TYPE gty_marm , "#EC
NEEDED gw_matid TYPE char22 ,"#EC NEEDED gt_marm_c TYPE STANDARD TABLE
OF gty_marm , "#EC NEEDED gw_marm_c TYPE gty_marm ,"#EC NEEDED gt_marm_t
TYPE STANDARD TABLE OF gty_marm , "#EC NEEDED gt_marm_c\_t TYPE STANDARD
TABLE OF gty_marm ,"#EC NEEDED gw_marm_t TYPE gty_marm , "#EC NEEDED
gw_marm_c\_t TYPE gty_marm ,"#EC NEEDED gt_db_refdoc TYPE
zewm_db_ref_doc_tt , "#EC NEEDED gw_bag_sum TYPE char46,"#EC NEEDED
gw_bag TYPE char46, "#EC NEEDED gt_sr_no TYPE TABLE OF /scwm/hu_ident
,"#EC NEEDED gw_sr_no TYPE /scwm/hu_ident , "#EC NEEDED gw_zzexp_srno
TYPE numc10 ,"#EC NEEDED gw_flag TYPE char1 , "#EC NEEDED gw_flag1 TYPE
char1 ,"#EC NEEDED gt_t005u TYPE TABLE OF t005u , "#EC NEEDED gt_adrc
TYPE zewm_adrc_tt ,"#EC NEEDED gt_adrc_temp TYPE zewm_adrc_tt , "#EC
NEEDED gt_but020 TYPE zewm_but020_tt ,"#EC NEEDED gt_but020_temp TYPE
zewm_but020_tt , "#EC NEEDED gt_plant TYPE TABLE OF /scwm/t300_md ,"#EC
NEEDED gt_lnumt TYPE TABLE OF /scwm/t300t , "#EC NEEDED gt_db_proci_o
TYPE zewm_db_proci_o1_tt ,"zewm_db_proci_o\_tt , "#EC NEEDED
gw_db_proci_o TYPE zewm_db_proci_o1_st ,"zewm_db_proci_o\_st , "#EC
NEEDED gt_db_proch_o TYPE zewm_db_proch_o\_tt ,"#EC NEEDED
gt_db_proch_o\_temp TYPE zewm_db_proch_o\_tt , "#EC NEEDED gw_wbn TYPE c
.

DATA: gw_control_parameters TYPE ssfctrlop, gt_otf_data_temp TYPE
ssfcrescl, gt_otf_data TYPE tsfotf, gw_memory_id TYPE char26.

DATA : lw_ordim_o TYPE gty_ordim_o , "#EC NEEDED lt_tu_dlv1 TYPE TABLE
OF /scwm/tu_dlv,"#EC NEEDED lw_tu_dlv TYPE /scwm/tu_dlv, "#EC NEEDED
lw_tu_dlv1 TYPE /scwm/tu_dlv ,"#EC NEEDED lw_waveitm TYPE
zewm_waveitm_st, "#EC NEEDED \* LW_WAVEHDR TYPE /SCWM/WAVEHDR,"#EC
NEEDED lw_rfc_des TYPE tvarv_val , "#EC NEEDED lw_ordim_c TYPE
gty_ordim_o,"#EC NEEDED \* lt_ordim_c TYPE zewm_ordim_o\_tt , "#EC
NEEDED lw_vsolm TYPE /scwm/ltap_vsolm ."#EC NEEDED

DATA : lw_flag TYPE c, lw_cnt TYPE i, lt_ordim_tmp TYPE STANDARD TABLE
OF lty_ordim_tmp, lw_ordim_tmp TYPE lty_ordim_tmp, lw_tabix TYPE
sy-tabix, lw_index TYPE sy-tabix . DATA : lt_ordim_o\_temp TYPE TABLE OF
gty_ordim_o, lw_ordim_o\_temp TYPE gty_ordim_o, lt_ordim_c\_sb TYPE
TABLE OF gty_ordim_c\_sb, lw_ordim_c\_sb TYPE gty_ordim_c\_sb. DATA :
lt_ordim_c\_temp TYPE zewm_ordim_o\_tt, lw_ordim_c\_temp LIKE LINE OF
lt_ordim_c\_temp. DATA : gt_value TYPE zewm_tt_prog_table, "#EC NEEDED
gt_field TYPE rsdsselopt_t,"#EC NEEDED gw_value TYPE zewm_prog_table,
"#EC NEEDED gw_field TYPE rsdsselopt,"#EC NEEDED gw_repid TYPE sy-repid
, "#EC NEEDED gw_duct TYPE c,"#EC NEEDED lw_prog_table TYPE
zewm_prog_table.

DATA: gw_rfc_dest TYPE tvarv_val, gw_open_qty_so TYPE kwmeng,
gw_open_qty_sto TYPE lfimg, gw_unit TYPE meins, gw_vbeln TYPE vbeln.

FIELD-SYMBOLS: `<gfs_huhdr>`{=html} TYPE gty_huhdr, "#EC NEEDED
`<gfs_gmhuhdr>`{=html} TYPE gty_gmhuhdr."#EC NEEDED

FIELD-SYMBOLS : `<lfs_ordim_c>`{=html} TYPE gty_ordim_o. "#EC NEEDED
FIELD-SYMBOLS: `<lfs_waveitm>`{=html} TYPE zewm_waveitm_st. CONSTANTS :
gc_x TYPE c VALUE 'X' , gc_smartform1 TYPE tdsfname VALUE
'Z_SCWM_MPN_POLYMER', gc_stat TYPE c VALUE 'C', gc_erp TYPE char3 VALUE
'ERP', gc_doccat_pdo TYPE /scdl/dl_doccat VALUE 'PDO', gc_refitemno_10
TYPE /scdl/dl_refitemno VALUE '10', gc_vbeln TYPE char5 VALUE 'VBELN' ,
gc_dlv TYPE char10 VALUE 'DELIVERYNO', gc_boenum TYPE char6 VALUE
'BOENUM' , gc_boe_sez TYPE char7 VALUE 'BOE_SEZ' , gc_msg TYPE char7
VALUE 'MESSAGE', gc_dest TYPE char20 VALUE 'ZECC_DEST_TRUSTED', gc_p
TYPE c VALUE 'P' , gc_len TYPE i VALUE 15 , gc_len1 TYPE i VALUE 20 ,
gc_len2 TYPE i VALUE 50 , gc_len3 TYPE i VALUE 25 , gc_len4 TYPE i VALUE
125 .

CONSTANTS : gc_name TYPE char20 VALUE 'ZECC_DEST_TRUSTED', "#EC NEEDED
gc_bag TYPE char3 VALUE 'BAG',"#EC NEEDED gc_statr TYPE char1 VALUE 'R'
, "#EC NEEDED gc_stat1 TYPE n VALUE '1' ,"#EC NEEDED gc_state TYPE char1
VALUE 'E' , gc_stat_c TYPE char1 VALUE 'C' , "#EC NEEDED gc_tart7 TYPE
/scwm/lvs_trart VALUE '7' ,"#EC NEEDED gc_tcode TYPE char14 VALUE
'/SCWM/CANCPICK' , "#EC NEEDED gc_tart2 TYPE /scwm/lvs_trart VALUE
'2',"#EC NEEDED gc_lgnum(5) TYPE c VALUE 'LGNUM', "#EC NEEDED lc_pdo
TYPE char3 VALUE 'PDO'. \* BOC by MSAT_050 on 20.04.2018 15:20:08 DATA:
lt_zhigh TYPE TABLE OF zhigh, lw_polyster_flag TYPE char1, lv_zhigh TYPE
zhigh. CONSTANTS: lc_repid TYPE repid VALUE 'SKIP_POLYMER_LOGIC',
lc_zvariable TYPE zvariable VALUE 'SPART', lc_3040 TYPE char4 VALUE
'3040'. \* EOC by MSAT_050 on 20.04.2018 15:20:08

-   Added by SN Das CD:8079871(Start) TYPES : BEGIN OF
    lty_ewm_prog_table, repid TYPE zewm_prog_table-repid, lgnum TYPE
    zewm_prog_table-lgnum, zhigh TYPE zewm_prog_table-zhigh, END OF
    lty_ewm_prog_table.

    TYPES: BEGIN OF lty_zscm_exec_var, name TYPE rvari_vnam, numb TYPE
    tvarv_numb, lgnum TYPE /scwm/lgnum, active TYPE zactive_flag, END OF
    lty_zscm_exec_var.

    DATA : lw_ewm_prog_table TYPE lty_ewm_prog_table, lt_material TYPE
    zscm_mat_rpl_t, lw_material TYPE zscm_mat_rpl, lt_ref_mat_rpl TYPE
    zscm_ref_mat_rpl_t, lw_ref_mat_rpl TYPE zscm_ref_mat_rpl_s, lt_ret
    TYPE bapiret2_t, lw_ret TYPE bapiret2, lv_matnr TYPE matnr,
    lv_truck_no TYPE ytruck_no, " added by omkar more on 28.03.2025
    TR:AD2K911823 lv_transporter_name TYPE char35, " added by omkar more
    on 28.03.2025 TR:AD2K911823 lt_zscm_exec_var TYPE TABLE OF
    lty_zscm_exec_var, lw_zscm_exec_var TYPE lty_zscm_exec_var.

    CONSTANTS: lc_zecc_dest_trusted TYPE rvari_vnam VALUE
    'ZECC_DEST_TRUSTED', lc_zscm_mpn_truck_licplt TYPE rvari_vnam VALUE
    'ZSCM_MPN_TRUCK_LICPLT'.

    FIELD-SYMBOLS : `<lfs_db_proci_o>`{=html} TYPE zewm_db_proci_o1_st.
    \*Added by SN Das CD:8079871(End)

    CLEAR : gt_docid\[\] , gt_ordim_o\[\] , gt_ordim_c\[\] ,
    gt_huhdr\[\] , gt_ordim_o2\[\] , gt_who\[\] , gt_whohu\[\] ,
    gw_no_wbn . CLEAR : gw_veh_num , gw_lic_plate , gw_tu_num , gw_who ,
    gw_who1 , gw_whohu , gw_output ,gw_sr_act ,gw_boe_avail ,
    gw_boe_flg.

    SELECT \* FROM zewm_prog_table INTO lw_prog_table UP TO 1 ROWS WHERE
    repid EQ gc_dest. ENDSELECT. IF sy-subrc EQ 0. lw_rfcdest =
    lw_prog_table-zlow. ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT' EXPORTING input =
    i_report_no IMPORTING output = i_report_no.

    " SOC BY OMKAR MORE ON 28.03.2025 TR:AD2K911823 IF i_lgnum IS NOT
    INITIAL. CLEAR lt_zscm_exec_var\[\]. SELECT name numb lgnum active
    FROM zscm_exec_var CLIENT SPECIFIED INTO TABLE lt_zscm_exec_var
    WHERE mandt = sy-mandt AND name = lc_zscm_mpn_truck_licplt AND
    active = abap_true. IF sy-subrc = 0.

        IF lw_rfcdest IS NOT INITIAL.
          CALL FUNCTION 'RFC_PING' DESTINATION lw_rfcdest
            EXCEPTIONS
              system_failure        = 1
              communication_failure = 2
              OTHERS                = 99.
          IF sy-subrc = 0.

            CALL FUNCTION 'FUNCTION_EXISTS' DESTINATION lw_rfcdest
              EXPORTING
                funcname           = 'Z_SCM_TRUCKNO_GET'
              EXCEPTIONS
                function_not_exist = 1
                OTHERS             = 2.
            IF sy-subrc = 0.
              CLEAR:lv_truck_no,lv_transporter_name.
              CALL FUNCTION 'Z_SCM_TRUCKNO_GET' DESTINATION lw_rfcdest
                EXPORTING
                  im_report_no        = i_report_no
                IMPORTING
                  ex_truck_no         = lv_truck_no
                  ex_trasnporter_name = lv_transporter_name.

            ENDIF.
          ENDIF.
        ENDIF.

    ENDIF.

    READ TABLE lt_zscm_exec_var TRANSPORTING NO FIELDS WITH KEY name =
    lc_zscm_mpn_truck_licplt lgnum = i_lgnum active = abap_true. IF
    sy-subrc = 0 AND lv_truck_no IS NOT INITIAL. CLEAR :
    gt\_/scwm/vehicle\[\]. SELECT veh_num lic_plate created_at FROM
    /scwm/vehicle CLIENT SPECIFIED INTO TABLE gt\_/scwm/vehicle WHERE
    mandt = sy-mandt AND lic_plate = lv_truck_no. IF sy-subrc EQ 0. SORT
    gt\_/scwm/vehicle BY created_at DESCENDING. CLEAR gw\_/scwm/vehicle.
    READ TABLE gt\_/scwm/vehicle INTO gw\_/scwm/vehicle INDEX 1. IF
    sy-subrc = 0. CLEAR gw_veh_num. gw_veh_num =
    gw\_/scwm/vehicle-veh_num. ENDIF. ENDIF. ELSE. SELECT SINGLE veh_num
    lic_plate "#EC WARNOK FROM /scwm/vehicle INTO (gw_veh_num ,
    gw_lic_plate ) WHERE veh_num_ext EQ i_report_no. IF sy-subrc NE 0.
    CLEAR:gw_veh_num,gw_lic_plate. ENDIF. ENDIF. ENDIF." EOC BY OMKAR
    MORE ON 28.03.2025 TR:AD2K911823

    IF gw_veh_num IS NOT INITIAL. SELECT veh_num veh_sr_act_num FROM
    /scwm/veh_sr_act INTO TABLE gt_sr_act WHERE veh_num = gw_veh_num.

    IF gt_sr_act IS NOT INITIAL AND sy-subrc EQ 0. SORT gt_sr_act BY
    veh_sr_act_num ASCENDING. DESCRIBE TABLE gt_sr_act LINES gw_index .
    READ TABLE gt_sr_act INTO gw_sr_act INDEX gw_index . IF sy-subrc
    EQ 0. gw_veh_sr_act = gw_sr_act-veh_sr_act_num . IF gw_veh_sr_act IS
    NOT INITIAL .

            SELECT SINGLE tu_num  tu_sr_act_num "#EC WARNOK  "#EC CI_SUBRC
               FROM /scwm/tu_veh INTO (gw_tu_num , gw_tu_sr_act  )
               WHERE veh_num EQ gw_veh_num
                AND veh_sr_act_num  = gw_veh_sr_act .

            IF gw_tu_num IS NOT INITIAL AND sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = gw_tu_num
                IMPORTING
                  output = gw_tu_num.

              SELECT docid                                  "#EC CI_SUBRC
               FROM  /scwm/tu_dlv
               INTO TABLE gt_docid
                WHERE lgnum EQ i_lgnum
                 AND tu_num EQ gw_tu_num
                 AND tu_sr_act_num  EQ gw_tu_sr_act.

              IF gt_docid IS NOT INITIAL .

                gt_docid_temp = gt_docid.
                SORT gt_docid_temp.
                DELETE ADJACENT DUPLICATES FROM gt_docid_temp COMPARING ALL FIELDS.

                IF gt_docid_temp IS NOT INITIAL.

                  SELECT  docid  refdoccat refdocno
                  FROM  /scdl/db_refdoc CLIENT SPECIFIED
                  INTO  TABLE gt_dlvno
                  FOR ALL ENTRIES IN gt_docid_temp
                  WHERE mandt   = sy-mandt
                  AND doccat    = gc_doccat_pdo
                  AND refdoccat = gc_erp
                  AND docid     = gt_docid_temp-table_line
                  AND refitemno = gc_refitemno_10.

                  IF gt_dlvno IS NOT INITIAL AND sy-subrc EQ 0.
                    LOOP AT gt_dlvno INTO gw_dlvno.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                        EXPORTING
                          input  = gw_dlvno-refdocno
                        IMPORTING
                          output = gw_delv_no-deliveryno.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = gw_delv_no-deliveryno
                        IMPORTING
                          output = gw_delv_no-deliveryno.

                      APPEND gw_delv_no TO gt_delv_no.
                      CLEAR : gw_dlvno, gw_delv_no.

                    ENDLOOP.

                    SORT gt_delv_no.
                    DELETE ADJACENT DUPLICATES FROM gt_delv_no.

                    IF lw_rfcdest IS NOT INITIAL.

                      CALL FUNCTION 'Z_PTC_EWM_BOE_DETAIL' DESTINATION lw_rfcdest
                        EXPORTING
                          it_delivery_in        = gt_delv_no
                        IMPORTING
                          et_error_log          = gt_err_log
                          ev_boe_flag           = gw_boe_flg
                        EXCEPTIONS
                          system_failure        = 1
                          communication_failure = 2.
                      IF sy-subrc NE 0.
                        lw_return-type    = gc_e.
                        lw_return-message = text-006.
                        APPEND lw_return TO et_return.
                        CLEAR lw_return.
                      ENDIF.

                    ENDIF.

                  ENDIF.

-           endif.

                  LOOP AT gt_err_log INTO gw_err_log .
                    IF gw_err_log-boenum IS NOT INITIAL OR  gw_err_log-boe_sez IS NOT INITIAL.
                      gw_boe_avail = 'X'.
                      EXIT.
                    ENDIF.
                  ENDLOOP.

-   Ordim_o \* SELECT "#EC CI_SUBRC lgnum tanum trart tostat matid
    stock_itmno charg meins vsolm vsola idplate vlpla sguid_hu vlenr
    nlber dguid_hu nlenr rdoccat rdocid ritmid tcode zzexp_srno FROM
    /scwm/ordim_o CLIENT SPECIFIED INTO TABLE gt_ordim_o FOR ALL ENTRIES
    IN gt_docid_temp WHERE mandt = sy-mandt AND rdoccat = lc_pdo AND
    rdocid EQ gt_docid_temp-table_line."#EC CI_NOFIRST IF sy-subrc EQ 0
    . "#EC NEEDED gw_flag = 'X'. ENDIF.

-   Confirm\_ o \*

                  SELECT                                    "#EC CI_SUBRC
                    lgnum
                     tanum
                     tapos
                     trart
                     tostat
                     matid
                     stock_itmno
                     charg
                     meins
                     vsolm
                     vsola
                     idplate
                     vlpla
                     sguid_hu
                     vlenr
                     nlber
                     dguid_hu
                     nlenr
                     rdoccat
                     rdocid
                     ritmid
                     tcode
                     zzexp_srno                           "#EC CI_NOFIRST
                   FROM /scwm/ordim_c CLIENT SPECIFIED
                   INTO TABLE gt_ordim_c_temp
                   FOR ALL ENTRIES IN  gt_docid_temp
                   WHERE  mandt = sy-mandt
                     AND rdocid EQ  gt_docid_temp-table_line
                     AND  tostat EQ gc_stat .
                  IF sy-subrc EQ 0.
                    SORT gt_ordim_c_temp BY tanum tapos rdocid ritmid .
                    DELETE ADJACENT DUPLICATES FROM gt_ordim_c_temp COMPARING tanum tapos rdocid ritmid .
                    LOOP AT gt_ordim_c_temp INTO gw_ordim_c_temp .
                      MOVE-CORRESPONDING gw_ordim_c_temp TO  gw_ordim_c .
                      APPEND  gw_ordim_c TO  gt_ordim_c .
                      CLEAR  gw_ordim_c .
                    ENDLOOP .
                    FREE gt_ordim_c_temp .                    "#EC NEEDED
                    gw_flag1 = 'X'.
                  ENDIF.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

    ENDIF. ENDIF. IF i_report_no IS NOT INITIAL AND i_lgnum IS NOT
    INITIAL . gw_output-tdsuffix1 = i_lgnum. gw_output-tdsuffix2 =
    i_report_no. ENDIF. " up to line 303

    IF gw_flag EQ 'X' OR gw_flag1 EQ 'X'.

    CLEAR : gt_marm\[\], gt_marm_c\[\] , gt_marm_c\_t\[\] ,
    gt_marm_t\[\].

    CLEAR :gt_marm\[\], gt_marm_c\[\] , gt_marm_c\_t\[\] ,
    gt_marm_t\[\].

    IF gw_tu_sr_act IS NOT INITIAL. gw_sr_act = gw_tu_sr_act . ENDIF.

    gt_ordim_o\_t\[\] = gt_ordim_o\[\] . CLEAR gt_ordim_o . READ TABLE
    gt_ordim_o\_t INTO lw_ordim_o INDEX 1. IF lw_ordim_o IS NOT INITIAL.
    CLEAR : lw_ordim_o. LOOP AT gt_ordim_o\_t INTO lw_ordim_o. CLEAR
    gw_matid. CALL FUNCTION '/SAPAPO/INC_CONVERT_GUIDS' EXPORTING
    iv_guid16 = lw_ordim_o-matid IMPORTING ev_guid22 = gw_matid. MOVE
    gw_matid TO gw_marm_t-matid. APPEND gw_marm_t TO gt_marm_t. CLEAR
    gw_marm_t-matid. ENDLOOP. FREE gt_ordim_o\_t . SORT gt_marm_t .
    DELETE ADJACENT DUPLICATES FROM gt_marm_t COMPARING ALL FIELDS . IF
    gt_marm_t IS NOT INITIAL. SELECT matid "#EC CI_SUBRC meinh umrez
    umren FROM /sapapo/marm INTO TABLE gt_marm FOR ALL ENTRIES IN
    gt_marm_t WHERE matid = gt_marm_t-matid AND meinh = gc_bag . IF
    sy-subrc EQ 0. SORT gt_marm."06.02 ENDIF. ENDIF.

        SELECT tu_num                                       "#EC CI_SUBRC
              FROM  /scwm/tu_dlv
              INTO  lw_tu_dlv-tu_num
               UP TO 1 ROWS
          WHERE docid = lw_ordim_o-rdocid.                    "#EC WARNOK
        ENDSELECT.
        IF lw_tu_dlv-tu_num  IS NOT INITIAL .
          IF gw_sr_act IS NOT INITIAL .

            SELECT *                                        "#EC CI_SUBRC
             FROM  /scwm/tu_dlv
             INTO TABLE lt_tu_dlv1
              WHERE tu_num = lw_tu_dlv-tu_num
               AND tu_sr_act_num = gw_sr_act .
            IF sy-subrc EQ 0.      "06.02
              SORT lt_tu_dlv1.
            ENDIF.
          ELSE.
            SELECT *                                        "#EC CI_SUBRC
           FROM  /scwm/tu_dlv
           INTO TABLE lt_tu_dlv1
            WHERE tu_num = lw_tu_dlv-tu_num .
            IF sy-subrc EQ 0.        "06.02
              SORT lt_tu_dlv1.
            ENDIF.
          ENDIF.
          IF gt_waveitm IS INITIAL.
            LOOP AT lt_tu_dlv1 INTO lw_tu_dlv1 .
              lw_waveitm-lgnum = lw_tu_dlv1-lgnum .
              lw_waveitm-tu_num  = lw_tu_dlv1-tu_num .
              lw_waveitm-rdocid = lw_tu_dlv1-docid .
              lw_waveitm-ritmid  = lw_tu_dlv1-itemid .
              APPEND lw_waveitm TO gt_waveitm.
              CLEAR lw_waveitm.
            ENDLOOP.
          ENDIF.
        ELSE.

          SELECT
           lgnum
           tanum
           trart
           tostat
           matid
           stock_itmno
           charg
           meins
           vsolm
           vsola
           idplate
           vlpla
           sguid_hu
           vlenr
           nlber
           dguid_hu
           nlenr
           rdoccat
           rdocid
           ritmid
           tcode
           zzexp_srno

    FROM /scwm/ordim_o INTO TABLE lt_ordim_o\_temp "#EC CI_SUBRC WHERE
    lgnum = lw_ordim_o-lgnum AND trart = gc_tart2 AND rdocid =
    lw_ordim_o-rdocid AND rdoccat = lw_ordim_o-rdoccat AND tostat = ' '.

          LOOP AT lt_ordim_o_temp INTO lw_ordim_o_temp.
            lw_waveitm-lgnum = lw_ordim_o_temp-lgnum .
            lw_waveitm-rdocid = lw_ordim_o_temp-rdocid .
            lw_waveitm-ritmid  = lw_ordim_o_temp-ritmid .
            APPEND lw_waveitm TO gt_waveitm.
          ENDLOOP.
          FREE lt_ordim_o_temp  .
        ENDIF.
        gt_waveitm_tmp = gt_waveitm.

        SORT gt_waveitm  BY lgnum tu_num tu_sr_act_num rdocid ritmid .
        DELETE ADJACENT DUPLICATES FROM gt_waveitm
        COMPARING lgnum tu_num tu_sr_act_num rdocid ritmid . "existing logic

        SORT gt_waveitm_tmp BY rdocid.
        DELETE gt_waveitm_tmp WHERE rdocid IS INITIAL.
        DELETE ADJACENT DUPLICATES FROM gt_waveitm_tmp COMPARING rdocid.

        IF gt_waveitm_tmp IS NOT INITIAL.
          SELECT                                            "#EC CI_SUBRC
            lgnum
            tanum
            trart
            tostat
            matid
            stock_itmno
            charg
            meins
            vsolm
            vsola
            idplate
            vlpla
            sguid_hu
            vlenr
            nlber
            dguid_hu
            nlenr
            rdoccat
            rdocid
            ritmid
            tcode
            zzexp_srno
          FROM /scwm/ordim_o CLIENT SPECIFIED
          INTO TABLE gt_ordim_o
          FOR ALL ENTRIES IN gt_waveitm_tmp
          WHERE mandt = sy-mandt
            AND rdoccat = lw_ordim_o-rdoccat
            AND rdocid = gt_waveitm_tmp-rdocid
            AND trart = gc_tart2
            AND tostat EQ ' ' .
          IF gt_ordim_o IS NOT INITIAL.
            SORT gt_ordim_o.       "06.02
          ENDIF.
        ENDIF.

-   ENDIF.

-   end of change 5/2/13\* SELECT SINGLE lnumt "#EC CI_SUBRC FROM
    /scwm/t300t INTO gw_lnumt WHERE spras = sy-langu AND lgnum =
    lw_ordim_o-lgnum. IF sy-subrc NE 0. CLEAR gw_lnumt. ENDIF.

        IF lw_ordim_o-rdocid IS NOT INITIAL
          AND gt_waveitm IS NOT INITIAL.

          gt_waveitm_tmp = gt_waveitm.

          SORT gt_waveitm_tmp BY rdocid.
          DELETE gt_waveitm_tmp WHERE rdocid IS INITIAL.
          DELETE ADJACENT DUPLICATES FROM gt_waveitm_tmp COMPARING rdocid.

          IF gt_waveitm_tmp IS NOT INITIAL.

            SELECT  docid
              itemid
              refdoccat
              doccat
              refdocno
              FROM  /scdl/db_refdoc CLIENT SPECIFIED
              INTO  TABLE gt_db_refdoc
              FOR ALL ENTRIES IN gt_waveitm_tmp
              WHERE mandt = sy-mandt
              AND  docid = gt_waveitm_tmp-rdocid
              AND   refdoccat  = gc_erp
              AND   doccat    = gc_doccat_pdo
              AND   refitemno = gc_refitemno_10.
            IF sy-subrc EQ 0.
              SORT  gt_db_refdoc BY refdocno .
              DELETE ADJACENT DUPLICATES FROM gt_db_refdoc COMPARING refdocno .
            ENDIF.

          ENDIF.

          LOOP AT gt_db_refdoc INTO gw_db_refdoc .
            APPEND gw_db_refdoc-refdocno TO gt_delv_no.
            CLEAR gw_db_refdoc .
          ENDLOOP .

          IF gt_delv_no IS NOT INITIAL.     "06.02
            SORT gt_delv_no BY deliveryno.
            DELETE ADJACENT DUPLICATES FROM gt_delv_no COMPARING deliveryno .

            IF lw_rfcdest IS NOT INITIAL.

              CALL FUNCTION 'ZLOG_GET_RP_DETAIL' DESTINATION lw_rfcdest "#EC CI_SUBRC
                EXPORTING
                  gt_delivery_in        = gt_delv_no
                  iv_report_no          = i_report_no
                  i_lgnum               = i_lgnum
                IMPORTING
                  gt_delv_detail_out    = gt_delv_st
                  gt_error_log          = gt_log_st
                  ev_wbn_status         = gw_no_wbn
                  ev_high               = lv_zhigh
                EXCEPTIONS                                    "#EC FB_RC
                  system_failure        = 1
                  communication_failure = 2.
              IF gw_no_wbn EQ 'X' AND gw_wbn EQ 'X'.
                lw_return-type = 'E'.
                lw_return-message = text-007.
                APPEND lw_return TO et_return.
                CLEAR lw_return.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

    ENDIF. " `<ordim_o>`{=html}

*--------------------------------------------* \* BOC by MSAT_050 on
20.04.2018 15:19:01 "Check if the Material is Polyster SELECT zhigh INTO
TABLE lt_zhigh FROM zewm_prog_table WHERE repid = lc_repid AND lgnum =
i_lgnum AND zvariable = lc_zvariable. IF sy-subrc = 0. SORT lt_zhigh BY
table_line. ENDIF. LOOP AT gt_delv_st INTO gw_delv_st. READ TABLE
lt_zhigh TRANSPORTING NO FIELDS WITH KEY table_line = gw_delv_st-spart.
IF sy-subrc = 0. lw_polyster_flag = abap_true. EXIT. ENDIF. ENDLOOP. \*
EOC by MSAT_050 on 20.04.2018 15:19:01 \*ordim_o which has records
linked in ordim_c for waveitm in ordim_o IF gt_waveitm IS NOT INITIAL.
CLEAR gt_ordim_c\_o. SORT gt_waveitm . DELETE ADJACENT DUPLICATES FROM
gt_waveitm COMPARING ALL FIELDS. gt_waveitm_tmp = gt_waveitm.

      SORT gt_waveitm_tmp BY rdocid.
      DELETE gt_waveitm_tmp WHERE rdocid IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM gt_waveitm_tmp COMPARING rdocid.

      IF gt_waveitm_tmp IS NOT INITIAL.
        SELECT lgnum                                      "#EC CI_SUBRC
            tanum
            tapos
            trart
            tostat
            matid
            stock_itmno
            charg
            meins
            vsolm
            vsola
            idplate
            vlpla
            sguid_hu
            vlenr
            nlber
            dguid_hu
            nlenr
            rdoccat
            rdocid
            ritmid
            tcode
            zzexp_srno
               FROM /scwm/ordim_c CLIENT SPECIFIED
               INTO TABLE  gt_ordim_c_temp
               FOR ALL ENTRIES IN gt_waveitm_tmp
              WHERE mandt = sy-mandt
              AND lgnum = i_lgnum
              AND rdoccat = lw_ordim_o-rdoccat
              AND rdocid = gt_waveitm_tmp-rdocid
              AND tostat = gc_stat_c .
        IF sy-subrc EQ 0.
          SORT gt_ordim_c_temp BY tanum tapos rdocid ritmid .
          DELETE ADJACENT DUPLICATES FROM gt_ordim_c_temp COMPARING tanum tapos rdocid ritmid .

          LOOP AT gt_ordim_c_temp INTO gw_ordim_c_temp .
            MOVE-CORRESPONDING gw_ordim_c_temp TO gw_ordim_c_o .
            APPEND   gw_ordim_c_o TO  gt_ordim_c_o .
            CLEAR  gw_ordim_c_o .
          ENDLOOP.
          FREE gt_ordim_c_temp .
        ENDIF.
      ENDIF.
      CLEAR gt_ordim_c_o2 .

-   If Confirmed then cancelled \* IF gt_ordim_c\_o IS NOT INITIAL .

         APPEND LINES OF gt_ordim_c_o TO gt_ordim_c_o2 .
         DELETE gt_ordim_c_o2 WHERE trart NE gc_tart7 .
         DELETE gt_ordim_c_o WHERE trart NE gc_tart2 .
         SORT gt_ordim_c_o BY lgnum trart idplate.   "06.02

         LOOP AT  gt_ordim_c_o2 INTO gw_ordim_c_o2
                          WHERE lgnum = lw_ordim_o-lgnum
                             AND tostat = gc_stat_c
                             AND trart = gc_tart7
                            AND tcode EQ  gc_tcode .

           READ TABLE gt_ordim_c_o  INTO gw_ordim_c_o
           WITH  KEY
            lgnum = lw_ordim_o-lgnum
            trart = gc_tart2
            idplate = gw_ordim_c_o2-idplate  BINARY SEARCH.   "06.02
           IF sy-subrc EQ 0 .
             DELETE TABLE gt_ordim_c_o FROM gw_ordim_c_o .
           ENDIF.
         ENDLOOP.

    ENDIF.

    IF gt_ordim_c\_o IS NOT INITIAL. LOOP AT gt_ordim_c\_o INTO
    gw_ordim_c. MOVE-CORRESPONDING gw_ordim_c TO gw_ordim_o. APPEND
    gw_ordim_o TO gt_ordim_o. CLEAR gw_ordim_o. ENDLOOP. SORT gt_ordim_o
    BY tanum rdocid ritmid.

-      DELETE ADJACENT DUPLICATES FROM  gt_ordim_o COMPARING  tanum rdocid ritmid. "8-10-2014
        ENDIF.

    ENDIF.

    gt_ordim_c\_t = gt_ordim_c . CLEAR gt_ordim_c . SORT gt_ordim_c\_t
    BY tostat. "06.02
    *-----------------------------------------------------* READ TABLE
    gt_ordim_c\_t INTO lw_ordim_c WITH KEY tostat = gc_stat_c BINARY
    SEARCH."06.02

    IF lw_ordim_c IS NOT INITIAL. UNASSIGN `<lfs_ordim_c>`{=html}. LOOP
    AT gt_ordim_c\_t ASSIGNING `<lfs_ordim_c>`{=html} .

-      READ TABLE gt_ordim_c_t ASSIGNING <lfs_ordim_c> INDEX 1 .

-      IF sy-subrc EQ 0.
          CLEAR gw_matid.
          CALL FUNCTION '/SAPAPO/INC_CONVERT_GUIDS'
            EXPORTING
              iv_guid16 = <lfs_ordim_c>-matid
            IMPORTING
              ev_guid22 = gw_matid.
          MOVE gw_matid TO gw_marm_c_t-matid.
          APPEND gw_marm_c_t TO  gt_marm_c_t.
          CLEAR  gw_marm_c_t-matid.

-      ENDIF.
        ENDLOOP.
        FREE  gt_ordim_o_t .
        SORT gt_marm_c_t BY matid.
        DELETE gt_marm_c_t WHERE matid IS INITIAL.
        DELETE ADJACENT DUPLICATES FROM gt_marm_c_t COMPARING matid.

        IF gt_marm_c_t IS NOT INITIAL.
          SELECT  matid                                     "#EC CI_SUBRC
                   meinh
                   umrez
                   umren
                    FROM /sapapo/marm
                    INTO  TABLE gt_marm_c
            FOR ALL ENTRIES IN gt_marm_c_t
                    WHERE matid = gt_marm_c_t-matid
                      AND meinh = gc_bag  .

        ENDIF.

        IF gt_marm_c IS NOT INITIAL .
          APPEND LINES OF gt_marm_c TO gt_marm .
        ENDIF.

-   below logic for confirm data

-   IF who-wave IS INITIAL. SELECT tu_num "#EC CI_SUBRC FROM
    /scwm/tu_dlv UP TO 1 ROWS INTO lw_tu_dlv-tu_num WHERE docid =
    lw_ordim_c-rdocid."#EC WARNOK ENDSELECT. IF lw_tu_dlv-tu_num IS NOT
    INITIAL . IF gw_sr_act IS NOT INITIAL. SELECT \* "#EC CI_SUBRC FROM
    /scwm/tu_dlv INTO TABLE lt_tu_dlv1 WHERE tu_num = lw_tu_dlv-tu_num
    AND tu_sr_act_num = gw_sr_act . IF sy-subrc EQ 0."06.02 SORT
    lt_tu_dlv1. ENDIF. ELSE. SELECT \* "#EC CI_SUBRC FROM /scwm/tu_dlv
    INTO TABLE lt_tu_dlv1 WHERE tu_num = lw_tu_dlv-tu_num . IF sy-subrc
    EQ 0."06.02 SORT lt_tu_dlv1. ENDIF. ENDIF. IF gt_waveitm_c IS
    INITIAL. LOOP AT lt_tu_dlv1 INTO lw_tu_dlv1 . lw_waveitm-lgnum =
    lw_tu_dlv1-lgnum . lw_waveitm-tu_num = lw_tu_dlv1-tu_num .
    lw_waveitm-rdocid = lw_tu_dlv1-docid . lw_waveitm-ritmid =
    lw_tu_dlv1-itemid . APPEND lw_waveitm TO gt_waveitm_c. CLEAR
    lw_waveitm. ENDLOOP. ENDIF. ELSE.

         SELECT  lgnum
                    tanum
                    tapos
                    trart
                    tostat
                    matid
                    stock_itmno
                    charg
                    meins
                    vsolm
                    vsola
                    idplate
                    vlpla
                    sguid_hu
                    vlenr
                    nlber
                    dguid_hu
                    nlenr
                    rdoccat
                    rdocid
                    ritmid
                    tcode
                    zzexp_srno                             "#EC CI_SUBRC
         FROM /scwm/ordim_c
         INTO TABLE   gt_ordim_c_temp
          WHERE lgnum = lw_ordim_c-lgnum
         AND rdoccat = lw_ordim_c-rdoccat
         AND rdocid = lw_ordim_c-rdocid
         AND trart = gc_tart2
         AND tostat  = gc_stat_c.

         IF sy-subrc EQ 0.
           SORT gt_ordim_c_temp BY tanum tapos rdocid ritmid .
           DELETE ADJACENT DUPLICATES FROM gt_ordim_c_temp COMPARING tanum tapos rdocid ritmid .

           LOOP AT gt_ordim_c_temp INTO gw_ordim_c_temp .
             MOVE-CORRESPONDING gw_ordim_c_temp TO lw_ordim_c_temp .
             APPEND   lw_ordim_c_temp TO  lt_ordim_c_temp.
             CLEAR  lw_ordim_c_temp .
           ENDLOOP.
           FREE gt_ordim_c_temp .
         ENDIF.
         LOOP AT lt_ordim_c_temp INTO lw_ordim_c_temp.
           lw_waveitm-lgnum = lw_ordim_c_temp-lgnum .
           lw_waveitm-rdocid = lw_ordim_c_temp-rdocid .
           lw_waveitm-ritmid  = lw_ordim_c_temp-ritmid .
           APPEND lw_waveitm TO gt_waveitm_c.
           CLEAR lw_waveitm.
         ENDLOOP.
         FREE lt_ordim_o_temp  .

    ENDIF. IF gt_waveitm_c IS NOT INITIAL. SORT gt_waveitm_c. "BY lgnum
    tu_num tu_sr_act_num rdocid ritmid. DELETE ADJACENT DUPLICATES FROM
    gt_waveitm_c COMPARING ALL FIELDS.

         gt_waveitm_tmp = gt_waveitm_c.

         SORT gt_waveitm_tmp BY rdocid.
         DELETE gt_waveitm_tmp WHERE rdocid IS INITIAL.
         DELETE ADJACENT DUPLICATES FROM gt_waveitm_tmp COMPARING rdocid.

         IF gt_waveitm_tmp IS NOT INITIAL.
           SELECT lgnum                                    "#EC CI_SUBRC
             tanum
             tapos
             trart
             tostat
             matid
             stock_itmno
             charg
             meins
             vsolm
             vsola
             idplate
             vlpla
             sguid_hu
             vlenr
             nlber
             dguid_hu
             nlenr
             rdoccat
             rdocid
             ritmid
             tcode
             zzexp_srno
           FROM /scwm/ordim_c
           INTO TABLE gt_ordim_c_temp
           FOR ALL ENTRIES IN  gt_waveitm_tmp
           WHERE  lgnum = i_lgnum   "gt_waveitm_c-lgnum  " chang 25.07.15
           AND  rdocid =  gt_waveitm_tmp-rdocid
           AND rdoccat = lw_ordim_c-rdoccat
           AND tostat = gc_stat_c .
           IF sy-subrc EQ 0.
             SORT  gt_ordim_c_temp BY tanum tapos rdocid ritmid.
             DELETE ADJACENT DUPLICATES FROM gt_ordim_c_temp COMPARING tanum tapos rdocid ritmid.
             LOOP AT gt_ordim_c_temp INTO gw_ordim_c_temp .
               MOVE-CORRESPONDING gw_ordim_c_temp TO gw_ordim_c.
               APPEND gw_ordim_c TO  gt_ordim_c.
               CLEAR  gw_ordim_c.
             ENDLOOP .
             FREE gt_ordim_c_temp .
           ENDIF.

         ENDIF.
         CLEAR gt_ordim_c2 .

-   If Confirmed then cancelled \* IF gt_ordim_c IS NOT INITIAL . APPEND
    LINES OF gt_ordim_c TO gt_ordim_c2 . DELETE gt_ordim_c2 WHERE trart
    NE gc_tart7 . DELETE gt_ordim_c WHERE trart NE gc_tart2. SORT
    gt_ordim_c BY lgnum trart idplate. "06.02 LOOP AT gt_ordim_c2 INTO
    gw_ordim_c2 WHERE lgnum = lw_ordim_c-lgnum AND tostat = gc_stat_c
    AND trart = gc_tart7 AND tcode EQ gc_tcode .

             READ TABLE gt_ordim_c  INTO gw_ordim_c
                                             WITH  KEY
                                             lgnum = lw_ordim_c-lgnum
                                             trart = gc_tart2
                                             idplate = gw_ordim_c2-idplate BINARY SEARCH.  "06.02
             IF sy-subrc EQ 0 .
               DELETE TABLE gt_ordim_c FROM gw_ordim_c .
             ENDIF.

-        ENDLOOP.
           ENDIF.
           SORT gt_ordim_c  BY matid.
           DELETE gt_ordim_c WHERE tostat NE gc_stat_c ."8-10-14
           CLEAR  gt_ordim_o_add[].

           LOOP AT gt_ordim_c INTO gw_ordim_c.
             MOVE-CORRESPONDING gw_ordim_c TO gw_ordim_o_add.
             APPEND gw_ordim_o_add TO gt_ordim_o_add.
             CLEAR gw_ordim_o_add.
           ENDLOOP.
           SORT  gt_ordim_o_add BY tanum  rdocid ritmid.

-      DELETE ADJACENT DUPLICATES FROM gt_ordim_o_add COMPARING tanum rdocid ritmid. "6-10-14
        ENDIF.

-   ENDIF.

-   end of change 5/2/13\* SELECT SINGLE lnumt "#EC CI_SUBRC FROM
    /scwm/t300t INTO gw_lnumt WHERE spras = sy-langu AND lgnum =
    lw_ordim_c-lgnum. IF sy-subrc NE 0. CLEAR gw_lnumt."06.02 ENDIF.

        IF lw_ordim_c-rdocid IS NOT INITIAL
          AND  gt_waveitm_c IS NOT INITIAL.

          gt_waveitm_tmp = gt_waveitm_c.

          SORT gt_waveitm_tmp BY rdocid.
          DELETE gt_waveitm_tmp WHERE rdocid IS INITIAL.
          DELETE ADJACENT DUPLICATES FROM gt_waveitm_tmp COMPARING rdocid.

          IF gt_waveitm_tmp IS NOT INITIAL.

            SELECT docid                                    "#EC CI_SUBRC
                itemid
              refdoccat
              doccat
              refdocno
               FROM /scdl/db_refdoc CLIENT SPECIFIED
               INTO TABLE gt_db_refdoc
               FOR ALL ENTRIES IN  gt_waveitm_tmp
               WHERE mandt = sy-mandt
               AND doccat EQ gc_doccat_pdo   "PDO
               AND refdoccat = gc_erp
               AND docid =  gt_waveitm_tmp-rdocid
               AND refitemno EQ gc_refitemno_10. "10
            IF sy-subrc EQ 0.
              SORT gt_db_refdoc BY refdocno .
            ENDIF.

          ENDIF.

          DELETE ADJACENT DUPLICATES FROM gt_db_refdoc COMPARING refdocno .
          LOOP AT gt_db_refdoc INTO gw_db_refdoc .
            APPEND gw_db_refdoc-refdocno TO gt_delv_no.
            CLEAR gw_db_refdoc .
          ENDLOOP .

          IF gt_delv_no IS NOT INITIAL.
            SORT gt_delv_no BY deliveryno .
            DELETE ADJACENT DUPLICATES FROM gt_delv_no COMPARING deliveryno  .

            SELECT *
           FROM zewm_prog_table
           INTO lw_prog_table UP TO 1 ROWS
           WHERE repid EQ gc_dest.
            ENDSELECT.
            IF sy-subrc EQ 0.
              lw_rfcdest = lw_prog_table-zlow.
            ENDIF.

            IF lw_rfcdest IS NOT INITIAL.

              CALL FUNCTION 'ZLOG_GET_RP_DETAIL' DESTINATION lw_rfcdest "#EC CI_SUBRC
                EXPORTING
                  gt_delivery_in        = gt_delv_no
                  iv_report_no          = i_report_no
                  i_lgnum               = i_lgnum
                IMPORTING
                  gt_delv_detail_out    = gt_delv_st
                  gt_error_log          = gt_log_st
                  ev_wbn_status         = gw_no_wbn
                  ev_high               = lv_zhigh
                EXCEPTIONS                                    "#EC FB_RC
                  system_failure        = 1
                  communication_failure = 2.
              IF gw_no_wbn EQ 'X' AND gw_wbn EQ 'X'.
                lw_return-type = 'E'.
                lw_return-message = text-006.
                APPEND lw_return TO et_return.
                CLEAR lw_return.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

    ENDIF ."ls_ordim_c

-   end of change

    SORT gt_marm." BY matid. DELETE ADJACENT DUPLICATES FROM gt_marm
    COMPARING ALL FIELDS .

    DELETE gt_ordim_o WHERE tostat = gc_stat_c. SORT gt_ordim_o BY tanum
    rdocid ritmid. "rdocid. DELETE ADJACENT DUPLICATES FROM gt_ordim_o
    COMPARING tanum rdocid ritmid." 8-10-2014.

    IF gt_ordim_o\_add IS NOT INITIAL . SORT gt_ordim_o\_add BY tanum
    rdocid ritmid.

    gt_ordim_o\_add_t = gt_ordim_o\_add.

    SORT gt_ordim_o\_add_t BY dguid_hu. DELETE gt_ordim_o\_add_t WHERE
    dguid_hu IS INITIAL. DELETE ADJACENT DUPLICATES FROM
    gt_ordim_o\_add_t COMPARING dguid_hu.

    IF gt_ordim_o\_add_t IS NOT INITIAL.

         SELECT *                                          "#EC CI_SUBRC
         FROM /scwm/hu_ident
         INTO TABLE gt_sr_no
          FOR ALL ENTRIES IN gt_ordim_o_add_t
          WHERE guid_hu = gt_ordim_o_add_t-dguid_hu
          AND idart = gc_x ."'X'.
         IF sy-subrc EQ 0.
           SORT gt_sr_no BY guid_hu idart.    "06.02
         ENDIF.

    ENDIF.

    ENDIF.

    IF gt_ordim_o IS NOT INITIAL OR gt_ordim_o\_add IS NOT INITIAL .

-   Add confirmed data to final table . CLEAR : gw_ordim_o
    ,gw_ordim_o\_add. LOOP AT gt_ordim_o\_add INTO gw_ordim_o\_add.
    MOVE-CORRESPONDING gw_ordim_o\_add TO gw_ordim_o. IF gt_sr_no IS NOT
    INITIAL . READ TABLE gt_sr_no INTO gw_sr_no WITH KEY guid_hu =
    gw_ordim_o-dguid_hu idart = gc_x BINARY SEARCH. MOVE
    gw_sr_no-huident TO gw_ordim_o-zzexp_srno . ENDIF. APPEND gw_ordim_o
    TO gt_ordim_o. CLEAR gw_ordim_o. ENDLOOP. CLEAR gt_ordim_o\_add.
    ENDIF. SORT gt_ordim_o BY tanum rdocid ritmid.

-   DELETE ADJACENT DUPLICATES FROM gt_ordim_o COMPARING tanum rdocid
    ritmid. "06-10-2014 IF gt_waveitm_c IS NOT INITIAL. APPEND LINES OF
    gt_waveitm_c TO gt_waveitm. SORT gt_waveitm ." BY lgnum tu_num
    tu_sr_act_num rdocid ritmid . DELETE ADJACENT DUPLICATES FROM
    gt_waveitm COMPARING ALL FIELDS. SORT gt_waveitm BY lgnum tu_num
    tu_sr_act_num rdocid ritmid . FREE gt_waveitm_c . ENDIF. IF
    gt_it_waveitm IS INITIAL . APPEND LINES OF gt_waveitm TO
    gt_it_waveitm . ENDIF.

```{=html}
<!-- -->
```
    CLEAR  lw_vsolm.
    CLEAR gw_matid.
    CLEAR gw_bag .
    CLEAR gw_marm.

    IF gt_it_waveitm IS NOT INITIAL.
      SORT gt_it_waveitm BY rdocid.   "06.02
      SORT gt_waveitm BY rdocid.

      gt_it_waveitm_tmp = gt_it_waveitm.
      DELETE gt_it_waveitm_tmp WHERE rdocid IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM gt_it_waveitm_tmp COMPARING rdocid.

      IF gt_it_waveitm_tmp IS NOT INITIAL.

        SELECT docid
               transmeans_id
               /scwm/stop_seq
          FROM /scdl/db_trans
          INTO TABLE gt_trans
          FOR ALL ENTRIES IN gt_it_waveitm_tmp
          WHERE docid = gt_it_waveitm_tmp-rdocid.
        IF sy-subrc IS INITIAL.
          SORT gt_trans BY /scwm/stop_seq DESCENDING.
          LOOP AT gt_trans INTO gw_trans.
            CLEAR gw_waveitm.
            READ TABLE gt_waveitm INTO gw_waveitm WITH KEY rdocid = gw_trans-docid BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              APPEND gw_waveitm TO gt_waveitm_temp.
            ENDIF.
          ENDLOOP.
          REFRESH gt_waveitm .
          gt_waveitm  =  gt_waveitm_temp.
          FREE gt_waveitm_temp.
        ENDIF.

      ENDIF.

    ENDIF.

    SORT gt_waveitm.
    SORT gt_ordim_o BY nlber.
    LOOP AT gt_ordim_o INTO gw_ordim_o.

      IF gw_nlber NE gw_ordim_o-nlber.
        gw_nlber = gw_ordim_o-nlber.
        CLEAR lw_cnt.
      ENDIF.

      lw_cnt = lw_cnt + 1.

      lw_ordim_tmp-count       = lw_cnt.
      lw_ordim_tmp-lgnum       = gw_ordim_o-lgnum.
      lw_ordim_tmp-tanum       = gw_ordim_o-tanum.
      lw_ordim_tmp-trart       = gw_ordim_o-trart.
      lw_ordim_tmp-tostat      = gw_ordim_o-tostat.
      lw_ordim_tmp-matid       = gw_ordim_o-matid.
      lw_ordim_tmp-stock_itmno = gw_ordim_o-stock_itmno.
      lw_ordim_tmp-charg       = gw_ordim_o-charg.
      lw_ordim_tmp-meins       = gw_ordim_o-meins.
      lw_ordim_tmp-vsolm       = gw_ordim_o-vsolm.
      lw_ordim_tmp-vsola       = gw_ordim_o-vsola.
      lw_ordim_tmp-idplate     = gw_ordim_o-idplate.
      lw_ordim_tmp-sguid_hu    = gw_ordim_o-sguid_hu.
      lw_ordim_tmp-vlpla       = gw_ordim_o-vlpla.
      lw_ordim_tmp-vlenr       = gw_ordim_o-vlenr.
      lw_ordim_tmp-nlber       = gw_ordim_o-nlber.
      lw_ordim_tmp-dguid_hu    = gw_ordim_o-dguid_hu.
      lw_ordim_tmp-nlenr       = gw_ordim_o-nlenr.
      lw_ordim_tmp-rdoccat     = gw_ordim_o-rdoccat.
      lw_ordim_tmp-rdocid      = gw_ordim_o-rdocid.
      lw_ordim_tmp-ritmid      = gw_ordim_o-ritmid.
      lw_ordim_tmp-tcode       = gw_ordim_o-tcode.
      lw_ordim_tmp-zzexp_srno  = gw_ordim_o-zzexp_srno.

      APPEND lw_ordim_tmp TO lt_ordim_tmp.
      CLEAR: lw_ordim_tmp, gw_ordim_o.

    ENDLOOP.

    CLEAR gw_nlber.
    SORT lt_ordim_tmp BY count DESCENDING.
    READ TABLE lt_ordim_tmp INTO lw_ordim_tmp INDEX 1.
    IF sy-subrc = 0.
      gw_nlber = lw_ordim_tmp-nlber.
    ENDIF.

\*\*\*\*\*\*\*\*\*End of code for Loading Bay by pwc_059 14/08/2014

-   SORT gt_ordim_o BY tanum vlpla zzexp_srno. SORT gt_ordim_o BY lgnum
    rdocid charg. "Charg Added By Kalpesh/Ravindra 16.12.2025
    AD2K912273"06.02 LOOP AT gt_waveitm INTO gw_waveitm . CLEAR
    lw_vsolm. CLEAR gw_bag. CLEAR gw_bag_sum . CLEAR gw_vlpla .

    READ TABLE gt_ordim_o INTO gw_ordim_o WITH KEY lgnum =
    gw_waveitm-lgnum rdocid = gw_waveitm-rdocid. "BINARY SEARCH ."Binary
    search removed because unsorted batch data going to print "06.02

    IF sy-subrc EQ 0 . CLEAR lw_index. LOOP AT gt_ordim_o INTO
    gw_ordim_o "#EC CI_NESTED WHERE lgnum = gw_waveitm-lgnum AND rdocid
    = gw_waveitm-rdocid .

         lw_index = sy-tabix .
         IF gw_ordim_o-zzexp_srno IS NOT INITIAL
           AND lw_flag EQ ' '.
           gw_zzexp_srno =  gw_ordim_o-zzexp_srno .
           lw_flag = gc_x .
         ENDIF.
         lw_index = lw_index + 1.
         READ TABLE gt_ordim_o INTO gw_ordim_o2 INDEX lw_index  .
         CALL FUNCTION '/SAPAPO/INC_CONVERT_GUIDS'  "Conversion of matid
              EXPORTING
              iv_guid16 = gw_ordim_o-matid
              IMPORTING
               ev_guid22 = gw_matid.

         READ TABLE gt_marm INTO gw_marm WITH KEY matid = gw_matid.
         IF sy-subrc NE 0.
           CLEAR gw_marm.
         ENDIF.

         CLEAR  :  gw_ordim_o2-vsolm .
         gw_ordim_o2-rdocid =  gw_ordim_o-rdocid .
         gw_ordim_o2-ritmid =  gw_ordim_o-ritmid .
         lw_vsolm = lw_vsolm  +  gw_ordim_o-vsolm.
         gw_ordim_o2-vsolm =  gw_ordim_o-vsolm .
         gw_ordim_o-vsolm = lw_vsolm.
         gw_ordim_o2-meins = gw_ordim_o-meins .
         gw_ordim_o2-lgnum = gw_ordim_o-lgnum .
         gw_ordim_o2-matid = gw_ordim_o-matid .
         gw_ordim_o2-nlber =  gw_ordim_o-nlber .

         IF  gw_marm-umren IS NOT INITIAL
           AND gw_marm-umrez IS NOT INITIAL.
           gw_bag = ( gw_ordim_o2-vsolm *
            ( gw_marm-umren / gw_marm-umrez ) ). "lv_total_quant
           IF  gw_bag IS NOT INITIAL.
             SHIFT gw_bag RIGHT DELETING TRAILING '.'.
             SHIFT gw_bag LEFT DELETING LEADING ' '.

-   Carton sum \* gw_bag_sum = gw_bag + gw_bag_sum. gw_ordim_o-vsola =
    gw_bag_sum . ENDIF. ENDIF.

          IF gw_ordim_o2-vlpla <> gw_ordim_o-vlpla .

            MOVE gw_ordim_o-zzexp_srno  TO gw_ordim_o-stock_itmno .
            MOVE gw_zzexp_srno TO gw_ordim_o-zzexp_srno.
            APPEND gw_ordim_o TO gt_ordim_o2.
            CLEAR :gw_bag,gw_bag_sum,gw_ordim_o2.
            CLEAR lw_vsolm.
            CLEAR gw_bag.
            CLEAR gw_bag_sum .
            CLEAR gw_vlpla .
            CLEAR lw_flag .
            CLEAR gw_zzexp_srno.

-        ELSEIF  GW_ORDIM_O-VLPLA EQ GW_ORDIM_O2-VLPLA ."#EC BOOL_OK
            ELSE.
              IF  gw_ordim_o2-charg <> gw_ordim_o-charg .
                MOVE gw_ordim_o-zzexp_srno  TO gw_ordim_o-stock_itmno  .
                MOVE gw_zzexp_srno TO gw_ordim_o-zzexp_srno.
                APPEND gw_ordim_o TO gt_ordim_o2.
                CLEAR :gw_bag,gw_bag_sum,gw_ordim_o2.
                CLEAR lw_vsolm.
                CLEAR gw_bag.
                CLEAR gw_bag_sum .
                CLEAR gw_vlpla .
                CLEAR lw_flag .
                CLEAR gw_zzexp_srno.
              ENDIF.
            ENDIF.

          ENDLOOP .
          IF gw_ordim_o  IS NOT INITIAL .
            IF gw_ordim_o-vlpla EQ gw_ordim_o2-vlpla .
              CLEAR lw_flag .
              MOVE gw_ordim_o-zzexp_srno  TO gw_ordim_o-stock_itmno  .
              MOVE gw_zzexp_srno TO gw_ordim_o-zzexp_srno.
              APPEND  gw_ordim_o TO gt_ordim_o2.
              CLEAR gw_ordim_o2.
              CLEAR gw_zzexp_srno.
            ENDIF.
          ENDIF.
        ENDIF.

    ENDLOOP.

    IF gw_ordim_o IS NOT INITIAL . IF gw_ordim_o-vlpla EQ
    gw_ordim_o2-vlpla . CLEAR lw_flag . IF gw_ordim_o-zzexp_srno IS NOT
    INITIAL . MOVE gw_ordim_o-zzexp_srno TO gw_ordim_o-stock_itmno .
    ENDIF. APPEND gw_ordim_o TO gt_ordim_o2. CLEAR gw_zzexp_srno. ENDIF.
    ENDIF.

    SORT gt_ordim_o2 BY tanum rdocid ritmid.

    DELETE ADJACENT DUPLICATES FROM gt_ordim_o2 COMPARING tanum rdocid
    ritmid . SORT gt_ordim_o2 BY rdocid. LOOP AT gt_waveitm ASSIGNING
    `<lfs_waveitm>`{=html}. "INTO gw_waveitm. READ TABLE gt_ordim_o2
    INTO gw_ordim_o2 WITH KEY rdocid = `<lfs_waveitm>`{=html}-rdocid
    BINARY SEARCH. IF sy-subrc IS NOT INITIAL.

-      DELETE TABLE gt_waveitm FROM gw_waveitm.
          CLEAR: <lfs_waveitm>.
        ENDIF.

    ENDLOOP.

    DELETE gt_waveitm WHERE rdocid IS INITIAL.

    REFRESH gt_it_waveitm. gt_it_waveitm = gt_waveitm. SORT gt_ordim_o2
    BY tanum.

-   BOC by MSAT_050 on 20.07.2018 16:07:10 IF lw_polyster_flag IS
    INITIAL . gt_ordim_o\[\] = gt_ordim_o2\[\]. ENDIF.

-   EOC by MSAT_050 on 20.07.2018 16:07:10 SORT gt_db_refdoc BY
    refdocno." docid .
    *----------------------------------------------------------* IF
    gt_ordim_o IS NOT INITIAL. "06.02 gt_ordim_o\_t\[\] = gt_ordim_o\[\]
    . SORT gt_ordim_o\_t BY rdocid. DELETE gt_ordim_o\_t WHERE rdocid IS
    INITIAL. DELETE ADJACENT DUPLICATES FROM gt_ordim_o\_t COMPARING
    rdocid.

        IF gt_ordim_o_t IS NOT INITIAL.

          SELECT
            docid
            docno
            itemno
            productno
            qty
            stock_owner
            refdocno_so
            FROM /scdl/db_proci_o
            INTO TABLE  gt_db_proci_o
            FOR ALL ENTRIES IN gt_ordim_o_t
            WHERE docid = gt_ordim_o_t-rdocid.
          IF sy-subrc EQ 0.
            SORT  gt_db_proci_o BY docid docno productno stock_owner refdocno_so.
            DELETE ADJACENT DUPLICATES FROM gt_db_proci_o COMPARING docid docno productno stock_owner refdocno_so.

            SELECT  docno
              partnerto_id
            FROM /scdl/db_proch_o
            CLIENT SPECIFIED
            INTO TABLE  gt_db_proch_o
            FOR ALL ENTRIES IN gt_db_proci_o
            WHERE mandt = sy-mandt
            AND  doccat = lc_pdo
            AND  docno = gt_db_proci_o-docno. "#EC CI_NOFIRST  "index reqd

            IF sy-subrc EQ 0 .
              gt_db_proch_o_temp = gt_db_proch_o.
              SORT gt_db_proch_o_temp BY partnerto_id.
              DELETE gt_db_proch_o_temp WHERE partnerto_id IS INITIAL.
              DELETE ADJACENT DUPLICATES FROM gt_db_proch_o_temp COMPARING partnerto_id.
              IF gt_db_proch_o_temp IS NOT INITIAL.
                SELECT name_org1
                  name_first
                  partner
                  partner_guid
                FROM but000
                INTO TABLE gt_but000
                  FOR ALL ENTRIES IN gt_db_proch_o_temp
                WHERE partner_guid = gt_db_proch_o_temp-partnerto_id.
                IF sy-subrc EQ 0.
                  SELECT  partner  addrnumber
                   FROM but020
                   INTO TABLE gt_but020
                   FOR ALL ENTRIES IN  gt_but000
                   WHERE partner = gt_but000-partner.
                  IF sy-subrc EQ 0.
                    gt_but020_temp = gt_but020.
                    SORT gt_but020_temp BY addrnumber.
                    DELETE gt_but020_temp WHERE addrnumber IS INITIAL.
                    DELETE ADJACENT DUPLICATES FROM gt_but020_temp COMPARING addrnumber.
                    IF gt_but020_temp IS NOT INITIAL.
                      SELECT addrnumber
                        city1
                        country
                        region
                      FROM adrc
                      INTO TABLE gt_adrc
                       FOR ALL ENTRIES IN gt_but020_temp
                      WHERE addrnumber = gt_but020_temp-addrnumber.
                      IF sy-subrc EQ 0.
                        gt_adrc_temp = gt_adrc.
                        SORT gt_adrc_temp BY country region.
                        DELETE gt_adrc_temp WHERE country IS INITIAL OR region IS INITIAL.
                        DELETE ADJACENT DUPLICATES FROM gt_adrc_temp COMPARING country region.

                        IF gt_adrc_temp IS NOT INITIAL.

                          SELECT *
                         FROM t005u
                         INTO TABLE gt_t005u
                         FOR ALL ENTRIES IN gt_adrc_temp
                         WHERE spras = sy-langu
                         AND land1 = gt_adrc_temp-country
                         AND bland = gt_adrc_temp-region.
                          IF sy-subrc NE 0.
                            REFRESH gt_t005u.
                          ENDIF.
                        ENDIF.

                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            SELECT *
            FROM /scwm/t300t
            INTO TABLE gt_lnumt
            FOR ALL ENTRIES IN gt_ordim_o
            WHERE spras = sy-langu
            AND lgnum = gt_ordim_o-lgnum.
            IF sy-subrc EQ 0.
              SORT gt_lnumt.
            ENDIF.

            SELECT *
                FROM /scwm/t300_md
                INTO TABLE  gt_plant
                FOR ALL ENTRIES IN gt_ordim_o
                WHERE lgnum = gt_ordim_o-lgnum.
            IF sy-subrc EQ 0.
              SORT gt_plant.
            ENDIF.

          ENDIF.

        ENDIF.

    ENDIF.

    CLEAR : lw_ordim_o , gt_log_st ,lw_rfc_des . SORT gt_ordim_o BY
    lgnum rdocid. " 06.02 READ TABLE gt_ordim_o INTO lw_ordim_o WITH KEY
    lgnum = gw_waveitm-lgnum rdocid = gw_waveitm-rdocid BINARY SEARCH .
    IF sy-subrc EQ 0.

        SELECT "SINGLE
          docid
          docno
          itemno
          productno
          qty
          stock_owner
          refdocno_so UP TO 1 ROWS
          FROM /scdl/db_proci_o
          INTO  gw_db_proci_o
          WHERE docid = lw_ordim_o-rdocid .                   "#EC WARNOK
        ENDSELECT.
        IF sy-subrc = 0.

          SELECT SINGLE                                     "#EC CI_SUBRC
            name_org1
            name_first
            partner
            partner_guid
            FROM but000
            INTO gw_but0001
            WHERE partner  LIKE gw_db_proci_o-stock_owner .   "#EC WARNOK
          IF gw_but0001 IS INITIAL.
            CLEAR gw_but0001.          "06.02
          ENDIF.
        ENDIF.

    ENDIF. CLEAR gw_flag. CLEAR gw_flag1. ENDIF.

    FREE : gt_docid , gt_who . CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING formname = gc_smartform1

-   VARIANT            = ' '

-   DIRECT_CALL        = ' '

    IMPORTING fm_name = gw_fm_name EXCEPTIONS no_form = 1
    no_function_module = 2 OTHERS = 3.

    IF sy-subrc \<\> 0. lw_return-type = 'E'. lw_return-message =
    text-011. APPEND lw_return TO et_return. CLEAR lw_return.

    ENDIF. " continue from here

    IF gt_ordim_o IS NOT INITIAL .

    IF i_lgnum IS NOT INITIAL.

        LOOP AT gt_db_refdoc INTO gw_db_refdoc.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gw_db_refdoc-refdocno
            IMPORTING
              output = gw_db_refdoc-refdocno.

          gw_vbeln = gw_db_refdoc-refdocno.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = gw_vbeln
            IMPORTING
              output = gw_vbeln.


          CLEAR : gw_open_qty_so, gw_open_qty_sto, gw_unit, gw_db_refdoc-refdocno.

          IF lw_rfcdest IS NOT INITIAL.

            CALL FUNCTION 'Z_LOG_GET_SO_STO_OPEN_QTY' DESTINATION lw_rfcdest "#EC CI_SUBRC
              EXPORTING
                iv_delivery_no   = gw_vbeln
              IMPORTING
                ev_open_qty_so   = gw_open_qty_so
                ev_open_qty_sto  = gw_open_qty_sto
                ev_unit          = gw_unit.

          ENDIF.

          READ TABLE gt_ordim_o INTO gw_ordim_o WITH KEY rdocid = gw_db_refdoc-docid.
          IF sy-subrc = 0.

            CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'          "#EC CI_SUBRC
              EXPORTING
                input                = gw_ordim_o-vsolm
                unit_in              = gw_ordim_o-meins
                unit_out             = gw_unit
              IMPORTING
                output               = gw_ordim_o-vsolm
              EXCEPTIONS
                conversion_not_found = 1
                division_by_zero     = 2
                input_invalid        = 3
                output_invalid       = 4
                overflow             = 5
                type_invalid         = 6
                units_missing        = 7
                unit_in_not_found    = 8
                unit_out_not_found   = 9
                OTHERS               = 10.

            IF lw_polyster_flag IS INITIAL.       " added by kpit_064 for polyster not applicable  by abdul wasim
              IF gw_open_qty_so IS NOT INITIAL OR gw_open_qty_sto IS NOT INITIAL.
                IF gw_ordim_o-vsolm > gw_open_qty_so AND gw_open_qty_so IS NOT INITIAL.
                  lw_return-type = 'E'.
                  lw_return-message = text-012.
                  APPEND lw_return TO et_return.
                  CLEAR lw_return.

-            MESSAGE text-012 TYPE 'E'.
                ENDIF.

                IF gw_ordim_o-vsolm > gw_open_qty_sto AND gw_open_qty_sto IS NOT INITIAL.
                  lw_return-type = 'E'.
                  lw_return-message = text-012.
                  APPEND lw_return TO et_return.
                  CLEAR lw_return.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

    ENDIF. \*\*\*SOC BRILO_007 Dt 31/05/2019 For getting Storage Bin for
    MPN Print

    FREE gt_ordim_o\_t . gt_ordim_o\_t = gt_ordim_o. SORT gt_ordim_o\_t
    BY lgnum vlenr. DELETE ADJACENT DUPLICATES FROM gt_ordim_o\_t
    COMPARING lgnum vlenr. DELETE gt_ordim_o\_t WHERE lgnum IS INITIAL
    OR vlenr IS INITIAL.

    IF gt_ordim_o\_t IS NOT INITIAL AND lv_zhigh IS NOT INITIAL.

        SELECT lgnum
               procty
               vlpla
               vlenr
               nlpla
          FROM /scwm/ordim_c
          CLIENT SPECIFIED
          INTO TABLE lt_ordim_c_sb
          FOR ALL ENTRIES IN gt_ordim_o_t
          WHERE mandt = sy-mandt
            AND lgnum = gt_ordim_o_t-lgnum
            AND vlenr = gt_ordim_o_t-vlenr. "Secondary Index SH Used
        IF sy-subrc = 0.
          SORT lt_ordim_c_sb BY lgnum vlenr.
          DELETE lt_ordim_c_sb WHERE procty NE lc_3040
                                  OR nlpla <> lv_zhigh+0(18).
        ENDIF.

    ENDIF. FREE gt_ordim_o\_t. "Soc Suraj Mutha 16/04/2024 DATA:
    lt_huident TYPE /scwm/tt_huident, lw_huident TYPE /scwm/s_huident,
    lt_huhdr TYPE /scwm/tt_huhdr_int, lt_huhdr_t TYPE
    /scwm/tt_huhdr_int, lw_huhdr TYPE /scwm/s_huhdr_int, lt_hutree TYPE
    /scwm/tt_hutree, lt_hutree_t TYPE /scwm/tt_hutree, lw_hutree TYPE
    /scwm/tt_hutree, lv_lines TYPE i. IF lw_polyster_flag EQ abap_true.

        gt_ordim_o_t = gt_ordim_o.
        SORT gt_ordim_o_t BY vlenr.
        DELETE ADJACENT DUPLICATES FROM gt_ordim_o_t COMPARING vlenr.
        DELETE gt_ordim_o_t WHERE vlenr IS INITIAL.

        LOOP AT gt_ordim_o_t INTO gw_ordim_o.
          lw_huident-lgnum = i_lgnum.
          lw_huident-huident = gw_ordim_o-vlenr.
          APPEND lw_huident TO lt_huident.
          CLEAR lw_huident.
        ENDLOOP.

        CALL FUNCTION '/SCWM/HU_READ_MULT'
          EXPORTING
            it_huident         = lt_huident

-       IT_GUID_HU         =

-       IV_LOCK            =

-       iv_lgnum           = i_lgnum

-       IV_AUTHORITY_CHECK = ' '

-       IV_TOP             =

-       IV_COMPLETE        =
          IMPORTING
            et_huhdr           = lt_huhdr

-       ET_HUITM           =
            et_hutree          = lt_hutree

-       ET_HIGH            =

-       ET_HUREF           =

-       ET_IDENT           =

-       ET_GUID_HU         =
          EXCEPTIONS
            wrong_input        = 1
            not_possible       = 2
            OTHERS             = 3.
        IF sy-subrc <> 0.

-   Implement suitable error handling here ENDIF. ENDIF. "EOC Suraj
    Mutha 16/04/2024 \*\*\*EOC BRILO_007 Dt 31/05/2019 For getting
    Storage Bin for MPN Print

    LOOP AT gt_ordim_o INTO gw_ordim_o. MOVE-CORRESPONDING gw_ordim_o TO
    gw_ordim_o\_tt. ***SOC BRILO_007 Dt 31/05/2019 For getting Storage
    Bin for MPN Print READ TABLE lt_ordim_c\_sb INTO lw_ordim_c\_sb WITH
    KEY lgnum = gw_ordim_o-lgnum vlenr = gw_ordim_o-vlenr BINARY SEARCH.
    IF sy-subrc = 0. CLEAR gw_ordim_o\_tt-vlpla. gw_ordim_o\_tt-vlpla =
    lw_ordim_c\_sb-vlpla. ENDIF. ***EOC BRILO_007 Dt 31/05/2019 For
    getting Storage Bin for MPN Print IF lw_polyster_flag EQ abap_true.
    CLEAR: lt_huhdr_t\[\] , lt_hutree_t\[\]. lt_huhdr_t\[\] =
    lt_huhdr\[\]. lt_hutree_t\[\] = lt_hutree\[\]. DELETE lt_huhdr_t
    WHERE huident NE gw_ordim_o-vlenr. READ TABLE lt_huhdr_t INTO
    lw_huhdr INDEX 1. IF sy-subrc IS INITIAL. DELETE lt_hutree_t WHERE
    guid_parent NE lw_huhdr-guid_hu. DESCRIBE TABLE lt_hutree_t LINES
    lv_lines. MOVE lv_lines TO gw_ordim_o\_tt-vsola . ENDIF. ENDIF.
    APPEND gw_ordim_o\_tt TO gt_ordim_o\_tt.
    CLEAR:gw_ordim_o\_tt,gw_ordim_o. ENDLOOP.

    gw_control_parameters-no_dialog = 'X'. gw_control_parameters-getotf
    = abap_true. gw_control_parameters-preview = space.
    gw_control_parameters-langu = sy-langu. gw_output-tdprinter =
    'PDF1'. gw_output-tdnewid = 'X'. gw_output-tddelete = space.

-   BREAK-POINT.

-   Added by SN Das CD:8079871(Start) SELECT repid lgnum zhigh FROM
    zewm_prog_table INTO lw_ewm_prog_table UP TO 1 ROWS WHERE repid =
    'ZSCM_RPL_PRODUCTCODE' AND lgnum = i_lgnum AND zhigh = 'X'.

    ENDSELECT.

    IF sy-subrc = 0. LOOP AT gt_db_proci_o ASSIGNING
    `<lfs_db_proci_o>`{=html}. lw_material-matnr =
    `<lfs_db_proci_o>`{=html}-productno. APPEND lw_material TO
    lt_material. CLEAR : lw_material. ENDLOOP. IF
    `<lfs_db_proci_o>`{=html} IS ASSIGNED.UNASSIGN
    `<lfs_db_proci_o>`{=html}.ENDIF.
    \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\* CALL FUNCTION 'RFC_PING'
    DESTINATION lw_rfcdest EXCEPTIONS system_failure = 1
    communication_failure = 2 OTHERS = 99. IF sy-subrc = 0."RFC_PING
    CALL FUNCTION 'FUNCTION_EXISTS' DESTINATION lw_rfcdest EXPORTING
    funcname = 'ZSCM_RPL_PRODUCT' EXCEPTIONS function_not_exist = 1
    OTHERS = 2. IF sy-subrc = 0."FUNCTION_EXISTS CALL FUNCTION
    'ZSCM_RPL_PRODUCT' DESTINATION lw_rfcdest EXPORTING it_material =
    lt_material\[\] IMPORTING et_ref_mat_rpl = lt_ref_mat_rpl\[\]
    et_return = lt_ret\[\] EXCEPTIONS language_not_maintain = 1. IF
    sy-subrc = 0. LOOP AT gt_db_proci_o ASSIGNING
    `<lfs_db_proci_o>`{=html}. lv_matnr =
    `<lfs_db_proci_o>`{=html}-productno. CALL FUNCTION
    'CONVERSION_EXIT_MATN1_INPUT' EXPORTING input = lv_matnr IMPORTING
    output = lv_matnr EXCEPTIONS length_error = 1 OTHERS = 2. READ TABLE
    lt_ref_mat_rpl INTO lw_ref_mat_rpl WITH KEY matnr = lv_matnr. IF
    sy-subrc = 0. `<lfs_db_proci_o>`{=html}-productno =
    `<lfs_db_proci_o>`{=html}-productno && '/' && lw_ref_mat_rpl-maktx.
    ENDIF. ENDLOOP. ENDIF. ENDIF."IF sy-subrc = 0."FUNCTION_EXISTS
    ENDIF."IF sy-subrc = 0."RFC_PING
    \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\* ENDIF.

-   Added by SN Das CD:8079871(End) CALL FUNCTION gw_fm_name EXPORTING
    control_parameters = gw_control_parameters output_options =
    gw_output user_settings = gc_x who = i_lgnum whohu = gt_whohu

-     ORDIM_O            = GT_ORDIM_O

-     ORDIM_C            = GT_ORDIM_C
          sr_act             = gw_tu_sr_act
          gw_lnumt           = gw_lnumt
          gw_but0001         = gw_but0001
          gw_duct            = gw_duct
          im_polyster_flag   = lw_polyster_flag
        IMPORTING
          job_output_info    = gt_otf_data_temp
          gw_nlber           = gw_nlber
        TABLES
          gt_db_proci_o      = gt_db_proci_o
          gt_db_proch_o      = gt_db_proch_o
          gt_t005u           = gt_t005u
          gt_adrc            = gt_adrc
          gt_ordim_o         = gt_ordim_o_tt
          gt_db_refdoc       = gt_db_refdoc
          gt_it_waveitm      = gt_it_waveitm
          gt_but000          = gt_but000
          gt_but020          = gt_but020
          gt_plant           = gt_plant
          gt_lnumt           = gt_lnumt
          gt_delv_st         = gt_delv_st
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

    IF sy-subrc \<\> 0. lw_return-type = 'E'. lw_return-message =
    text-011. APPEND lw_return TO et_return. CLEAR lw_return. ENDIF.

    IF NOT gt_otf_data_temp-otfdata IS INITIAL.

        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            format                = 'PDF'

-       MAX_LINEWIDTH         = 132

-       ARCHIVE_INDEX         = ' '

-       COPYNUMBER            = 0

-       ASCII_BIDI_VIS2LOG    = ' '

-       PDF_DELETE_OTFTAB     = ' '

-       PDF_USERNAME          = ' '

-       PDF_PREVIEW           = ' '

-       USE_CASCADING         = ' '
          IMPORTING
            bin_filesize          = gw_binsize
            bin_file              = e_xstring
          TABLES
            otf                   = gt_otf_data_temp-otfdata "gt_otf_data_final
            lines                 = gt_lines
          EXCEPTIONS
            err_max_linewidth     = 1
            err_format            = 2
            err_conv_not_possible = 3
            err_bad_otf           = 4
            OTHERS                = 5.

    ENDIF.

    ELSE.

-   MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'S'. lw_return-type = 'E'.
    lw_return-message = text-013. APPEND lw_return TO et_return. CLEAR
    lw_return. ENDIF.

e_mimetype = 'application/pdf'. CONCATENATE i_report_no 'pdf' INTO
e_filename SEPARATED BY '.'. IF e_xstring IS INITIAL. lw_return-type =
'E'. lw_return-message = text-014. APPEND lw_return TO et_return. CLEAR
lw_return. ENDIF.

ENDFUNCTION.
