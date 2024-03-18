subroutine img_pal_init
! --------------------------------------------------------------
! Palette RVB continue.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
use imgyom
#include"implicit_r8i4.h"
cgcoulc(:)='None'
do jpal=1,jppal
  if(jpal == 1) then
    !
    ! -------------------------------------------------
    ! Palette REF.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='REF'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=042 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=094 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=145 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=196 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=247 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=212
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=109
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=007
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=095 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=147 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=198 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=210 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=159 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=107 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=107 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=056 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=005 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=046
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=097
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=149
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
  elseif(jpal == 2) then
    !
    ! -------------------------------------------------
    ! Palette AQUABLUE.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='AQUABLUE'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=150
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=084 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=223
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    !-------------------------------------------------
    ! On rappelle vers le blanc, pour créer des couleurs moins saturées.
    !-------------------------------------------------
    !
    do jseuil=1,ncoul(jpal)
      zmoy=0.
      do jc=1,3
        zmoy=zmoy+(255.-rvbpal(jc,jseuil,jpal))/3.
      enddo
      zfrac=0.3*zmoy/255.
      do jc=1,3
        rvbpal(jc,jseuil,jpal)=zfrac*255.+(1.-zfrac)*rvbpal(jc,jseuil,jpal)
      enddo
    enddo
  elseif(jpal == 3) then
    !
    ! -------------------------------------------------
    ! Palette VEG.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='VEG'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=189 ; rvbpal(2,ncoul(jpal),jpal)=122 ; rvbpal(3,ncoul(jpal),jpal)=019
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=076 ; rvbpal(2,ncoul(jpal),jpal)=140 ; rvbpal(3,ncoul(jpal),jpal)=074
  elseif(jpal == 4) then
    !
    ! -------------------------------------------------
    ! Palette SAFO_TMER.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='SAFO_TMER'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=095 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=102
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=020 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=134
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=193
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=024 ; rvbpal(3,ncoul(jpal),jpal)=236
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=106 ; rvbpal(3,ncoul(jpal),jpal)=170
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=112 ; rvbpal(3,ncoul(jpal),jpal)=017
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=149 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=184 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=205 ; rvbpal(2,ncoul(jpal),jpal)=144 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=145 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=188 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 5) then
    !
    ! -------------------------------------------------
    ! Palette TS.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='TS'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=132 ; rvbpal(2,ncoul(jpal),jpal)=045 ; rvbpal(3,ncoul(jpal),jpal)=206
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=144 ; rvbpal(2,ncoul(jpal),jpal)=066 ; rvbpal(3,ncoul(jpal),jpal)=211
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=157 ; rvbpal(2,ncoul(jpal),jpal)=087 ; rvbpal(3,ncoul(jpal),jpal)=216
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=169 ; rvbpal(2,ncoul(jpal),jpal)=108 ; rvbpal(3,ncoul(jpal),jpal)=221
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=181 ; rvbpal(2,ncoul(jpal),jpal)=129 ; rvbpal(3,ncoul(jpal),jpal)=226
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=194 ; rvbpal(2,ncoul(jpal),jpal)=150 ; rvbpal(3,ncoul(jpal),jpal)=231
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=206 ; rvbpal(2,ncoul(jpal),jpal)=171 ; rvbpal(3,ncoul(jpal),jpal)=235
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=218 ; rvbpal(2,ncoul(jpal),jpal)=192 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=213 ; rvbpal(3,ncoul(jpal),jpal)=245
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=243 ; rvbpal(2,ncoul(jpal),jpal)=234 ; rvbpal(3,ncoul(jpal),jpal)=250
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=129 ; rvbpal(2,ncoul(jpal),jpal)=005 ; rvbpal(3,ncoul(jpal),jpal)=065
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=142 ; rvbpal(2,ncoul(jpal),jpal)=030 ; rvbpal(3,ncoul(jpal),jpal)=084
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=154 ; rvbpal(2,ncoul(jpal),jpal)=055 ; rvbpal(3,ncoul(jpal),jpal)=103
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=167 ; rvbpal(2,ncoul(jpal),jpal)=080 ; rvbpal(3,ncoul(jpal),jpal)=122
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=105 ; rvbpal(3,ncoul(jpal),jpal)=141
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=192 ; rvbpal(2,ncoul(jpal),jpal)=130 ; rvbpal(3,ncoul(jpal),jpal)=160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=205 ; rvbpal(2,ncoul(jpal),jpal)=155 ; rvbpal(3,ncoul(jpal),jpal)=179
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=217 ; rvbpal(2,ncoul(jpal),jpal)=180 ; rvbpal(3,ncoul(jpal),jpal)=198
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=205 ; rvbpal(3,ncoul(jpal),jpal)=217
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=242 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=236
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=026 ; rvbpal(2,ncoul(jpal),jpal)=026 ; rvbpal(3,ncoul(jpal),jpal)=026
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=051 ; rvbpal(2,ncoul(jpal),jpal)=051 ; rvbpal(3,ncoul(jpal),jpal)=051
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=077 ; rvbpal(2,ncoul(jpal),jpal)=077 ; rvbpal(3,ncoul(jpal),jpal)=077
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=102 ; rvbpal(2,ncoul(jpal),jpal)=102 ; rvbpal(3,ncoul(jpal),jpal)=102
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=128 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=128
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=153 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=153
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=179
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=204 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=204
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=230
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=026 ; rvbpal(2,ncoul(jpal),jpal)=026 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=051 ; rvbpal(2,ncoul(jpal),jpal)=051 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=077 ; rvbpal(2,ncoul(jpal),jpal)=077 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=102 ; rvbpal(2,ncoul(jpal),jpal)=102 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=128 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=153 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=204 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=065 ; rvbpal(2,ncoul(jpal),jpal)=163 ; rvbpal(3,ncoul(jpal),jpal)=023
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=046
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=103 ; rvbpal(2,ncoul(jpal),jpal)=181 ; rvbpal(3,ncoul(jpal),jpal)=069
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=122 ; rvbpal(2,ncoul(jpal),jpal)=191 ; rvbpal(3,ncoul(jpal),jpal)=093
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=141 ; rvbpal(2,ncoul(jpal),jpal)=200 ; rvbpal(3,ncoul(jpal),jpal)=116
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=160 ; rvbpal(2,ncoul(jpal),jpal)=209 ; rvbpal(3,ncoul(jpal),jpal)=139
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=218 ; rvbpal(3,ncoul(jpal),jpal)=162
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=198 ; rvbpal(2,ncoul(jpal),jpal)=227 ; rvbpal(3,ncoul(jpal),jpal)=185
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=217 ; rvbpal(2,ncoul(jpal),jpal)=237 ; rvbpal(3,ncoul(jpal),jpal)=209
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=232
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=026 ; rvbpal(3,ncoul(jpal),jpal)=026
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=051 ; rvbpal(3,ncoul(jpal),jpal)=051
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=077 ; rvbpal(3,ncoul(jpal),jpal)=077
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=102 ; rvbpal(3,ncoul(jpal),jpal)=102
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=128
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=153
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=179
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=204
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=230
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=248 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=023
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=141 ; rvbpal(3,ncoul(jpal),jpal)=046
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=069
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=250 ; rvbpal(2,ncoul(jpal),jpal)=166 ; rvbpal(3,ncoul(jpal),jpal)=093
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=251 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=116
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=192 ; rvbpal(3,ncoul(jpal),jpal)=139
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=162
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=253 ; rvbpal(2,ncoul(jpal),jpal)=217 ; rvbpal(3,ncoul(jpal),jpal)=185
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=254 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=209
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=254 ; rvbpal(2,ncoul(jpal),jpal)=242 ; rvbpal(3,ncoul(jpal),jpal)=232
  elseif(jpal == 6) then
    !
    ! -------------------------------------------------
    ! Palette orographique.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='OROG'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=156 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=033 ; rvbpal(2,ncoul(jpal),jpal)=189 ; rvbpal(3,ncoul(jpal),jpal)=024
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=148 ; rvbpal(2,ncoul(jpal),jpal)=165 ; rvbpal(3,ncoul(jpal),jpal)=107
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=180 ; rvbpal(2,ncoul(jpal),jpal)=180 ; rvbpal(3,ncoul(jpal),jpal)=180
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=200
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=231 ; rvbpal(2,ncoul(jpal),jpal)=107 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=239 ; rvbpal(2,ncoul(jpal),jpal)=040 ; rvbpal(3,ncoul(jpal),jpal)=041
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=140 ; rvbpal(2,ncoul(jpal),jpal)=008 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 7) then
    !
    ! -------------------------------------------------
    ! Palette CONTRASTE.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='CONTRASTE'
    ncoul(jpal)=0
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=180
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=042 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=094 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=145 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=196 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=247 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=212
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=109
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=095 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=147 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=198 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=224 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=191 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=064 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=045
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=097
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=149
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
    !
    ! Rose.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 8) then
    !
    ! -------------------------------------------------
    ! Palette arc-en-ciel.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='ARC-EN-CIEL'
    ncoul(jpal)=0
    ! 1.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=159
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=060 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=214
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    ! 2.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=046 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=114 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=187 ; rvbpal(3,ncoul(jpal),jpal)=255
    ! 3.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=199
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=135
    ! 4.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=063
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=063 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ! 5.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=131 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=199 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=238 ; rvbpal(3,ncoul(jpal),jpal)=000
    ! 6.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=178 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=119 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 9) then
    !
    ! -------------------------------------------------
    ! Palette NOIR-BLANC.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='NOIR-BLANC'
    ncoul(jpal)=0
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 10) then
    !
    ! -------------------------------------------------
    ! Palette FARBEN.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='FARBEN'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         254  ; rvbpal(2,ncoul(jpal),jpal)=         230  ; rvbpal(3,ncoul(jpal),jpal)=           0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         248  ; rvbpal(2,ncoul(jpal),jpal)=         165  ; rvbpal(3,ncoul(jpal),jpal)=          27
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         242  ; rvbpal(2,ncoul(jpal),jpal)=         112  ; rvbpal(3,ncoul(jpal),jpal)=          34
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         246  ; rvbpal(2,ncoul(jpal),jpal)=         172  ; rvbpal(3,ncoul(jpal),jpal)=         187
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         216  ; rvbpal(2,ncoul(jpal),jpal)=          89  ; rvbpal(3,ncoul(jpal),jpal)=         160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         239  ; rvbpal(2,ncoul(jpal),jpal)=          66  ; rvbpal(3,ncoul(jpal),jpal)=          34
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         215  ; rvbpal(2,ncoul(jpal),jpal)=          26  ; rvbpal(3,ncoul(jpal),jpal)=          33
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         191  ; rvbpal(2,ncoul(jpal),jpal)=          30  ; rvbpal(3,ncoul(jpal),jpal)=          36
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         138  ; rvbpal(2,ncoul(jpal),jpal)=           0  ; rvbpal(3,ncoul(jpal),jpal)=          25
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         159  ; rvbpal(2,ncoul(jpal),jpal)=         125  ; rvbpal(3,ncoul(jpal),jpal)=         185
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=          30  ; rvbpal(2,ncoul(jpal),jpal)=         177  ; rvbpal(3,ncoul(jpal),jpal)=         231
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=           0  ; rvbpal(2,ncoul(jpal),jpal)=         137  ; rvbpal(3,ncoul(jpal),jpal)=         207
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=           0  ; rvbpal(2,ncoul(jpal),jpal)=          95  ; rvbpal(3,ncoul(jpal),jpal)=         175
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=           0  ; rvbpal(2,ncoul(jpal),jpal)=          75  ; rvbpal(3,ncoul(jpal),jpal)=         143
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=           0  ; rvbpal(2,ncoul(jpal),jpal)=         165  ; rvbpal(3,ncoul(jpal),jpal)=         184
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         211  ; rvbpal(2,ncoul(jpal),jpal)=         225  ; rvbpal(3,ncoul(jpal),jpal)=         104
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         166  ; rvbpal(2,ncoul(jpal),jpal)=         207  ; rvbpal(3,ncoul(jpal),jpal)=          89
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=          58  ; rvbpal(2,ncoul(jpal),jpal)=         181  ; rvbpal(3,ncoul(jpal),jpal)=          74
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=           1  ; rvbpal(2,ncoul(jpal),jpal)=         127  ; rvbpal(3,ncoul(jpal),jpal)=          63
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=           0  ; rvbpal(2,ncoul(jpal),jpal)=          75  ; rvbpal(3,ncoul(jpal),jpal)=          28
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         254  ; rvbpal(2,ncoul(jpal),jpal)=         208  ; rvbpal(3,ncoul(jpal),jpal)=         158
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=         227  ; rvbpal(2,ncoul(jpal),jpal)=         152  ; rvbpal(3,ncoul(jpal),jpal)=          69
  elseif(jpal == 11) then
    !
    ! -------------------------------------------------
    ! Palette BLEU-BLANC-ROUGE.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLEU-BLANC-ROUGE'
    ncoul(jpal)=0
    !
    ! Noir.
    !
    ! ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 12) then
    !
    ! -------------------------------------------------
    ! Palette NOIR-BLEU-ROUGE-VERT-BLANC.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='NOIR-BLEU-ROUGE-VERT-BLANC'
    ncoul(jpal)=0
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 13) then
    !
    ! -------------------------------------------------
    ! Palette VIOLET-BLANC.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='VIOLET-BLANC'
    ncoul(jpal)=0
    !
    ! Violet.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=159
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Orange.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=152 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 14) then
    !
    ! -------------------------------------------------
    ! Palette BLANC-NOIR.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLANC-NOIR'
    ncoul(jpal)=0
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 15) then
    !
    ! -------------------------------------------------
    ! Palette JAUNE-ROUGE-ROSE.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='JAUNE-ROUGE-ROSE'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=214 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=131 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=090 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=048 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=034
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=076
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=117
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=158
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
  elseif(jpal == 16) then
    !
    ! -------------------------------------------------
    ! Palette BLEU-VERT-ROUGE.
    ! Palette inspirée de celle des pluies radar du site Keraunos (juin 2018).
    ! La palette Keraunos est
    !     255 255 255
    !     108 230 228
    !     068 158 236
    !     004 038 236
    !     116 246 076
    !     076 162 052
    !     076 159 053
    !     252 246 084
    !     228 188 065
    !     236 154 059
    !     236 054 036
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLEU-VERT-ROUGE'
    ncoul(jpal)=0
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=108 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=228
    !
    ! Bleu ciel.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=068 ; rvbpal(2,ncoul(jpal),jpal)=158 ; rvbpal(3,ncoul(jpal),jpal)=236
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=004 ; rvbpal(2,ncoul(jpal),jpal)=038 ; rvbpal(3,ncoul(jpal),jpal)=236
    !
    ! Vert fashion.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=076 ; rvbpal(2,ncoul(jpal),jpal)=162 ; rvbpal(3,ncoul(jpal),jpal)=052
    !
    ! Vert prairie.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=076 ; rvbpal(2,ncoul(jpal),jpal)=159 ; rvbpal(3,ncoul(jpal),jpal)=053
    !
    ! Vert d'eau.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=116 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=076
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=084
    !
    ! Jaune poussin.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=228 ; rvbpal(2,ncoul(jpal),jpal)=188 ; rvbpal(3,ncoul(jpal),jpal)=065
    !
    ! Orange.
    !
    !ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=154 ; rvbpal(3,ncoul(jpal),jpal)=059
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=152 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=054 ; rvbpal(3,ncoul(jpal),jpal)=036
  elseif(jpal == 17) then
    !
    ! -------------------------------------------------
    ! Palette BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE'
    ncoul(jpal)=0
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Bleu-vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=100 ; rvbpal(3,ncoul(jpal),jpal)=180
    !
    ! Vert-bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rose.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 18) then
    !
    ! -------------------------------------------------
    ! Palette BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE-BLANC.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE-BLANC'
    ncoul(jpal)=0
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Bleu-vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=100 ; rvbpal(3,ncoul(jpal),jpal)=180
    !
    ! Vert-bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rose.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 19) then
    !
    ! -------------------------------------------------
    ! Palette TbIR10.8µm
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='TbIR10.8µm'
    ncoul(jpal)=0
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Orange.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=152 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Violet.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=159
  elseif(jpal == 20) then
    !
    ! -------------------------------------------------
    ! Palette BLANC-BLEU-VERT-ROUGE.
    ! Palette inspirée de celle des pluies radar du site Keraunos (juin 2018).
    ! La palette Keraunos est
    !     255 255 255
    !     108 230 228
    !     068 158 236
    !     004 038 236
    !     116 246 076
    !     076 162 052
    !     076 159 053
    !     252 246 084
    !     228 188 065
    !     236 154 059
    !     236 054 036
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLANC-BLEU-VERT-ROUGE'
    ncoul(jpal)=0
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=108 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=228
    !
    ! Bleu ciel.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=068 ; rvbpal(2,ncoul(jpal),jpal)=158 ; rvbpal(3,ncoul(jpal),jpal)=236
    !
    ! Bleu.
    !
    ! ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=004 ; rvbpal(2,ncoul(jpal),jpal)=038 ; rvbpal(3,ncoul(jpal),jpal)=236
    !
    ! Vert fashion.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=076 ; rvbpal(2,ncoul(jpal),jpal)=162 ; rvbpal(3,ncoul(jpal),jpal)=052
    !
    ! Vert prairie.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=076 ; rvbpal(2,ncoul(jpal),jpal)=159 ; rvbpal(3,ncoul(jpal),jpal)=053
    !
    ! Vert d'eau.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=116 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=076
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=084
    !
    ! Jaune poussin.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=228 ; rvbpal(2,ncoul(jpal),jpal)=188 ; rvbpal(3,ncoul(jpal),jpal)=065
    !
    ! Orange.
    !
    !ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=154 ; rvbpal(3,ncoul(jpal),jpal)=059
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=152 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=054 ; rvbpal(3,ncoul(jpal),jpal)=036
  elseif(jpal == 21) then
    !
    ! -------------------------------------------------
    ! Palette JAUNE-ROUGE.
    ! Palette inspirée de celle des pluies radar du site Keraunos (juin 2018).
    ! La palette Keraunos est
    !     255 255 255
    !     108 230 228
    !     068 158 236
    !     004 038 236
    !     116 246 076
    !     076 162 052
    !     076 159 053
    !     252 246 084
    !     228 188 065
    !     236 154 059
    !     236 054 036
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='JAUNE-ROUGE'
    ncoul(jpal)=0
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=084
    !
    ! Jaune poussin.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=228 ; rvbpal(2,ncoul(jpal),jpal)=188 ; rvbpal(3,ncoul(jpal),jpal)=065
    !
    ! Orange.
    !
    !ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=154 ; rvbpal(3,ncoul(jpal),jpal)=059
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=152 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=054 ; rvbpal(3,ncoul(jpal),jpal)=036
  elseif(jpal == 22) then
    !
    ! -------------------------------------------------
    ! Palette BLANC-JAUNE-ROUGE-ROSE.
    ! -------------------------------------------------
    !
    cgcoulc(jpal)='BLANC-JAUNE-ROUGE-ROSE'
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=214 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=131 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=090 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=048 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=034
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=076
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=117
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=158
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
    !
    !-------------------------------------------------
    ! On rappelle vers le blanc, pour créer des couleurs moins saturées.
    !-------------------------------------------------
    !
    do jseuil=1,ncoul(jpal)
      zmoy=0.
      do jc=1,3
        zmoy=zmoy+(255.-rvbpal(jc,jseuil,jpal))/3.
      enddo
      zfrac=0.3*zmoy/255.
      do jc=1,3
        rvbpal(jc,jseuil,jpal)=zfrac*255.+(1.-zfrac)*rvbpal(jc,jseuil,jpal)
      enddo
    enddo
  else
    print*,'img_pal_init/ERREUR: nombre de palettes trop grand!...'
    call exit(1)
  endif
enddo
end
