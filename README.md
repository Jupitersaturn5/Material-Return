# Material-Return
Material Return (Outbound)


  METHOD if_rest_resource~get.
*************************************************************************
*                                                                       *
*    Class for Extraction of Material Return details From SAP.  *                                                                  *
*    Class name : ZCL_MAT_RETURN_PROVIDER                              *                                    *
*    DATE CREATED         : 12.06.2023                                  *
*                                                                       *
*************************************************************************
*-------------------------------------------------------Types declaration for mseg values---------------------------------------
    TYPES: BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             zeile TYPE mseg-zeile,
             werks TYPE mseg-werks,
             lifnr TYPE mseg-lifnr,
             ebeln TYPE mseg-ebeln,
             ebelp TYPE mseg-ebelp,
             matnr TYPE mseg-matnr,
             meins TYPE mseg-meins,
             menge TYPE mseg-menge,
             lfbnr TYPE mseg-lfbnr,
             bwart TYPE bwart,
           END OF ty_mseg.
*-------------------------------------------------------Types declaration for mkpf values---------------------------------------
    TYPES : BEGIN OF ty_mkpf,
              mblnr    TYPE mseg-mblnr,
              budat    TYPE mkpf-budat,
              le_vbeln TYPE mkpf-le_vbeln,
              bldat    TYPE mkpf-bldat,
              cpudt    TYPE mkpf-cpudt,
            END OF ty_mkpf.

    TYPES : BEGIN OF ty_mkpf1,
              mblnr    TYPE mseg-mblnr,
              budat    TYPE sy-datum,
              le_vbeln TYPE mkpf-le_vbeln,
              bldat    TYPE mkpf-bldat,
            END OF ty_mkpf1.

    TYPES : BEGIN OF ty_mkpf2,
              mblnr TYPE mseg-mblnr,
              budat TYPE mkpf-bldat,
            END OF ty_mkpf2.
*-------------------------------------------------------Types declaration for ekbe values---------------------------------------
    TYPES : BEGIN OF ty_ekbe,
              ebeln TYPE mseg-ebeln,
              ebelp TYPE mseg-ebelp,
              belnr TYPE ekbe-belnr,
              buzei TYPE ekbe-buzei,
            END OF ty_ekbe.
*-------------------------------------------------------Types declaration for lfa1 values---------------------------------------
    TYPES : BEGIN OF ty_lfa1,
              lifnr TYPE mseg-lifnr,
              name1 TYPE lfa1-name1,
            END OF ty_lfa1.
*-------------------------------------------------------Types declaration for makt values---------------------------------------
    TYPES : BEGIN OF ty_makt,
              matnr TYPE mseg-matnr,
              maktx TYPE makt-maktx,
            END OF ty_makt.
*-------------------------------------------------------Types declaration for ekko values---------------------------------------
    TYPES : BEGIN OF ty_ekko,
              ebeln TYPE mseg-ebeln,
              aedat TYPE ekko-aedat,
            END OF ty_ekko.
*-------------------------------------------------------Types declaration for final values---------------------------------------
    TYPES : BEGIN OF ty_final,
              recordtype(10) TYPE c,
              mrno           TYPE mseg-mblnr,
              mrno_line      TYPE mseg-zeile,
              mrdate         TYPE mkpf-budat,
              grnno          TYPE ekbe-belnr,
              grn_line       TYPE ekbe-buzei,
              grndate        TYPE mkpf-budat,
              branchid       TYPE mseg-werks,
              vendorcode     TYPE mseg-lifnr,
              vendorname     TYPE lfa1-name1,
              pono           TYPE mseg-ebeln,
              poitemlineno   TYPE mseg-ebelp,
              podate         TYPE ekko-aedat,
              supinvno       TYPE mkpf-le_vbeln,
              supinvdate     TYPE mkpf-bldat,
              itemcode       TYPE mseg-matnr,
              description    TYPE makt-maktx,
              uom            TYPE mseg-meins,
              grnqty         TYPE mseg-menge,
              returnqty      TYPE mseg-menge,
              total_return   TYPE mseg-menge,
              Modified_on   TYPE mkpf-budat,
              is_deleted(3)   TYPE c,
            END OF ty_final.
*-------------------------------------------------------Types declaration for find total return---------------------------------------
    TYPES : BEGIN OF ty_tot,
              lifnr  TYPE lifnr,
              matnr  TYPE matnr,
              return TYPE ktmng,
            END OF ty_tot.
*----------------------------------------------- Data declaration -----------------------------------
    DATA : lt_tot TYPE TABLE OF ty_tot,
           ls_tot TYPE ty_tot,
           a      TYPE ktmng.

    DATA : lt_mseg  TYPE TABLE OF ty_mseg,
           ls_mseg  TYPE ty_mseg,
           lt_mkpf  TYPE TABLE OF ty_mkpf,
           ls_mkpf  TYPE ty_mkpf,
           lt_ekko  TYPE TABLE OF ty_ekko,
           ls_ekko  TYPE  ty_ekko,
           lt_ekbe  TYPE TABLE OF ty_ekbe,
           ls_ekbe  TYPE ty_ekbe,
           lt_makt  TYPE TABLE OF ty_makt,
           ls_makt  TYPE ty_makt,
           lt_lfa1  TYPE TABLE OF ty_lfa1,
           ls_lfa1  TYPE ty_lfa1,
           lt_final TYPE TABLE OF ty_final,
           ls_final TYPE ty_final,
           lt_mkpf1 TYPE TABLE OF ty_mkpf1,
           ls_mkpf1 TYPE ty_mkpf1,
           lt_mkpf2 TYPE TABLE OF ty_mkpf2,
           ls_mkpf2 TYPE ty_mkpf2.

    DATA :   ls_json TYPE string.
    DATA(l_value) = mo_request->get_uri_query_parameter( 'AEDAT' ).            " For get delta values
*---------------------------------------------------------------------------------------------------------------------------------------------
    IF l_value IS NOT INITIAL.                       "Full Load
      SELECT mblnr
            zeile
            werks
            lifnr
            ebeln
            ebelp
            matnr
            meins
            menge
            lfbnr
            bwart FROM mseg INTO TABLE lt_mseg WHERE cpudt_mkpf = l_value AND ( bwart = '122' OR bwart = '123' OR bwart = '161'
                                                                                 OR bwart = '162'  OR bwart = '105').

    ELSE.                                           "Delta Load
      SELECT mblnr
              zeile
              werks
              lifnr
              ebeln
              ebelp
              matnr
              meins
              menge
              lfbnr
              bwart FROM mseg INTO TABLE lt_mseg WHERE bwart = '122' OR bwart = '123' OR bwart = '105' OR bwart = '161'
                                                                                                       OR bwart = '162'.
    ENDIF.

    SORT lt_mseg BY mblnr ebeln ebelp.
    IF lt_mseg IS NOT INITIAL.
      SELECT mblnr
             budat
             le_vbeln
             bldat
             cpudt FROM mkpf INTO TABLE lt_mkpf FOR ALL ENTRIES IN lt_mseg WHERE mblnr = lt_mseg-mblnr.

      SORT lt_mkpf BY mblnr.

      SELECT ebeln
             ebelp
             belnr
             buzei FROM ekbe INTO TABLE lt_ekbe FOR ALL ENTRIES IN lt_mseg WHERE ebeln = lt_mseg-ebeln AND ebelp = lt_mseg-ebelp AND bwart ='105'.
      SORT lt_ekbe BY ebeln ebelp.
      IF lt_ekbe IS NOT INITIAL.
        SELECT mblnr
               budat FROM mkpf INTO TABLE lt_mkpf2 FOR ALL ENTRIES IN lt_ekbe WHERE mblnr = lt_ekbe-belnr.
        SORT lt_mkpf2 BY mblnr.
      ENDIF.
      SELECT lifnr
             name1  FROM lfa1 INTO TABLE lt_lfa1 FOR ALL ENTRIES IN lt_mseg WHERE lifnr = lt_mseg-lifnr.
      SORT lt_lfa1 BY lifnr.

      SELECT matnr
             maktx FROM makt INTO TABLE lt_makt FOR ALL ENTRIES IN lt_mseg WHERE matnr = lt_mseg-matnr.
      SORT lt_makt BY matnr.

      SELECT  ebeln
              aedat FROM ekko INTO TABLE lt_ekko FOR ALL ENTRIES IN lt_mseg WHERE ebeln = lt_mseg-ebeln.
      SORT lt_ekko BY ebeln.
*-------------------------------------------Find a Total Return----------------------------------
      LOOP AT lt_mseg INTO ls_mseg.
        ls_tot-lifnr = ls_mseg-lifnr.
        ls_tot-matnr = ls_mseg-matnr.
        APPEND ls_tot TO lt_tot.
        CLEAR : ls_tot,ls_mseg.
      ENDLOOP.

      SORT lt_tot BY lifnr matnr.
      DELETE ADJACENT DUPLICATES FROM lt_tot COMPARING lifnr
                                                       matnr.
      LOOP AT lt_mseg INTO ls_mseg WHERE bwart = '122' OR bwart = '161'.

        READ TABLE lt_tot INTO ls_tot WITH KEY lifnr = ls_mseg-lifnr matnr = ls_mseg-matnr.
        IF sy-subrc IS INITIAL.
          a = ls_tot-return.
          ls_tot-return = a + ls_mseg-menge.

        ENDIF.
        MODIFY lt_tot FROM ls_tot TRANSPORTING return
                WHERE lifnr = ls_mseg-lifnr AND matnr = ls_mseg-matnr.
        CLEAR ls_mseg .

      ENDLOOP.
*--------------------------------------------------------------------------------------------------------------------------------
      LOOP AT lt_mseg INTO ls_mseg.

        READ TABLE lt_mseg INTO ls_mseg WITH KEY mblnr = ls_mseg-lfbnr bwart = '123' BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE lt_mseg INTO ls_mseg WITH KEY mblnr = ls_mseg-lfbnr bwart = '162' BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            IF ls_mseg-bwart = '122' OR ls_mseg-bwart = '161'.
              CLEAR ls_final.
              if l_value is not initial.
              ls_final-recordtype = Text-002.
              endif.
              ls_final-is_deleted = Text-004.
              ls_final-mrno = ls_mseg-mblnr.
              ls_final-mrno_line = ls_mseg-zeile.
              ls_final-branchid = ls_mseg-werks.
              ls_final-vendorcode = ls_mseg-lifnr.
              ls_final-pono = ls_mseg-ebeln.
              ls_final-poitemlineno = ls_mseg-ebelp.
              ls_final-itemcode = ls_mseg-matnr.
              ls_final-uom = ls_mseg-meins.
              ls_final-returnqty = ls_mseg-menge.

              READ TABLE lt_mkpf INTO ls_mkpf WITH KEY mblnr = ls_mseg-mblnr BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                ls_final-mrdate = ls_mkpf-budat.
                ls_final-supinvno = ls_mkpf-le_vbeln.
                ls_final-supinvdate = ls_mkpf-bldat.
                ls_final-modified_on = ls_mkpf-cpudt.
              ENDIF.

              READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_mseg-ebeln
                                                       ebelp = ls_mseg-ebelp BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                ls_final-grnno = ls_ekbe-belnr.
                ls_final-grn_line = ls_ekbe-buzei.

                READ TABLE lt_mkpf1 INTO ls_mkpf1 WITH KEY mblnr = ls_ekbe-belnr BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  ls_final-grndate = ls_mkpf1-budat.
                  READ TABLE lt_mseg INTO ls_mseg WITH KEY mblnr = ls_ekbe-belnr
                                                           ebeln = ls_ekbe-ebeln
                                                           ebelp = ls_ekbe-ebelp BINARY SEARCH.
                  IF sy-subrc IS INITIAL.

                    ls_final-grnqty = ls_mseg-menge.

                  ENDIF.
                ENDIF.
              ENDIF.

              READ TABLE lt_mkpf2 INTO ls_mkpf2 WITH KEY mblnr = ls_ekbe-belnr.
              IF sy-subrc IS INITIAL.
                ls_final-grndate = ls_mkpf2-budat.
              ENDIF.
              READ TABLE lt_tot INTO ls_tot WITH KEY lifnr = ls_mseg-lifnr matnr = ls_mseg-matnr.
              IF sy-subrc IS INITIAL.
                ls_final-total_return = ls_tot-return.
              ENDIF.

              READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_mseg-ebeln BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                ls_final-podate = ls_ekko-aedat.
              ENDIF.

              READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_mseg-lifnr BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                ls_final-vendorname = ls_lfa1-name1.
              ENDIF.

              READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_mseg-matnr BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                ls_final-description = ls_makt-maktx.
              ENDIF.
              APPEND ls_final TO lt_final.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR : ls_mseg,ls_mkpf,ls_ekbe,ls_lfa1,ls_makt,ls_ekko,ls_mkpf1.
      ENDLOOP.
    ENDIF.

    IF lt_final IS NOT INITIAL.
      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = lt_final
        RECEIVING
          r_json = ls_json.

    ELSE.
      CALL METHOD /ui2/cl_json=>serialize(
        EXPORTING
          data   = TEXT-001
        RECEIVING
          r_json = ls_json
      ).
    ENDIF.

    mo_response->create_entity( )->set_string_data( iv_data = ls_json ).

  ENDMETHOD.
