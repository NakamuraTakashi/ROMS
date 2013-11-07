
!!!====== ver 2013/01/08 ===============================================

!----- CPP defines -----------------------------------------------------

!#define TEST_GFW
!#define TEST_MFW

!!!*********************************************************************

  module mod_foodweb

    implicit none



  contains
  
!!!*********************************************************************

    subroutine foodweb_coral    &
!          input parameters
     &            (PFD            &   ! Photon flux density (umol m-2 s-1)
     &            ,NH4amb         &   ! NH4 concentration (umol L-1)
!          output parameters
     &            ,Pg             &   ! Gross photosynthesis rate (mmol m-2 s-1)
     &            ,R              &   ! Respiration rate (mmol m-2 s-1)
     &            ,Gn             &   ! Galcification rate (mmol m-2 s-1)
     &            ,DICuptake      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,TAuptake       &   ! TA  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,DOuptake       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,NO3uptake      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,NH4uptake      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,PO4uptake      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,LDOCuptake     &   ! TOC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,RDOCuptake     &   ! TON uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &             )

! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Caliculate dissolved nutrient flux between benthic  *
! *                                                                    *
! *                organisms and water column.                         *
! *                                                                    *
! ********************************************************************** 

      implicit none
      real(8), intent(in) ::      &
     &             PFD            &
     &            ,NH4amb         
! output parameters
      real(8), intent(out) ::     &
     &             Pg             &
     &            ,R              &
     &            ,Gn             &
     &            ,DICuptake      &
     &            ,TAuptake       &
     &            ,DOuptake       &
     &            ,NO3uptake      &
     &            ,NH4uptake      &
     &            ,PO4uptake      &
     &            ,LDOCuptake     &
     &            ,RDOCuptake

! --- C:N:P ratio of coral ---
! *** Animal ***
!      real(8), parameter :: nc=1./6.37 !G.Muller-Parker et al.(1994)
!      real(8), parameter :: pc=1./172. !G.Muller-Parker et al.(1994)
! *** Zooxanthellae ***
      real(8), parameter :: nc=1./19.66 !G.Muller-Parker et al.(1994)
      real(8), parameter :: pc=1./365. !G.Muller-Parker et al.(1994)
! --- Photosynthesis and Calcification Parameters (coral) ---
!      real(8), parameter :: pmax = 85.9 !referenced from Hata et al(2002)
!      real(8), parameter :: pIk  = 526.
!      real(8), parameter :: p0   = 0. !-24.8
!      real(8), parameter :: cmax = 14.5
!      real(8), parameter :: cIk  = 35.
!      real(8), parameter :: c0   = -1.7
      real(8), parameter :: pmax =  58.1 !1999 metabolism referenced from Kayanne et al(2005)
      real(8), parameter :: pIk  = 213.
      real(8), parameter :: p0   =  16.5 !-16.5
      real(8), parameter :: cmax =  10.4
      real(8), parameter :: cIk  =  30.
      real(8), parameter :: c0   =  -0.6
! --- DOC release parameters (coral) ---      
      real(8), parameter :: rrp =0.12 !Ratio between DOC release and photosynthesis fixed carbon (Ferrier-Pages etal.,2000)
      real(8), parameter :: dnr =3./13. !The night DOC release rate is 30% of those of the day (Crossland,1987)
      real(8), parameter :: lr  =1.0

      real(8) npref
      real(8) ldocn,ldocd

! --- Organic and Inorganic Production Rate -----------------

      Pg= pmax*tanh(PFD/pIk)/3600   !Light response curve [mmolC/m2/s]
      R = p0/3600

      Gn=(cmax*tanh(PFD/cIk)+c0)/3600   !Light response curve [mmolC/m2/s]
      
      TAuptake = 2*Gn
      DICuptake= Pg-R+Gn
      DOuptake = R -Pg
      
! --- Nutrient fluxes between water column and coral -----
      npref= ninter(NH4amb)

      NO3uptake=Pg*nc*npref         !Flux caliculated in Carbon assimilated by Photosynthesis times C:N:P ratio [mmol/m2/s]
      NH4uptake=Pg*nc*(1.-npref)-R*nc
      PO4uptake=(Pg-R)*pc

! *** Base DOC release rate(release rate of night time)
      ldocn=             &
     &        -622.d0    & !daily gross production [mmolC/m2/day]
     &        *rrp       &
     &        *dnr       &
     &        /12.d0/3600.d0 !assuming night is 12hrs [mmolC/m2/sec]
! *** DOC release rate
      ldocd=-Pg*rrp*(1.-dnr)/3600

      if(ldocd.lt.ldocn) ldocd=ldocn

      LDOCuptake=lr*ldocd ![mmolC/m2/s]
      RDOCuptake=(1.-lr)*ldocd ![mmolC/m2/s]

      return
    end subroutine foodweb_coral

!!!*********************************************************************


    subroutine foodweb_seagrass &
!          input parameters
     &            (PFD            &   ! Photon flux density (umol m-2 s-1)
     &            ,NH4amb         &   ! NH4 concentration (umol L-1)
!          output parameters
     &            ,Pg             &   ! Gross photosynthesis rate (nmol cm-2 s-1)
     &            ,R              &   ! Respiration rate (nmol cm-2 s-1)
     &            ,Gn             &   ! Galcification rate (nmol cm-2 s-1)
     &            ,DICuptake      &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,DOuptake       &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,NO3uptake      &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,NH4uptake      &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,PO4uptake      &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &             )

! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Caliculate dissolved nutrient flux between benthic  *
! *                                                                    *
! *                organisms and water column.                         *
! *                                                                    *
! ********************************************************************** 

      implicit none
      real(8), intent(in) ::      &
     &             PFD            &
     &            ,NH4amb          
! output parameters
      real(8), intent(out) ::     &
     &             Pg             &
     &            ,R              &
     &            ,Gn             &
     &            ,DICuptake      &
     &            ,DOuptake       &
     &            ,NO3uptake      &
     &            ,NH4uptake      &
     &            ,PO4uptake

! --- C:N:P ratio of seagrass ---
      real(8), parameter :: nc=27./599. !M.J.Atkinson and SV Smith(1983)
      real(8), parameter :: pc=1./599.
! --- Photosynthesis and Calcification Parameters (coral) ---
      real(8), parameter :: pmax =  51.3 !average of 2009 observation and nakamura
      real(8), parameter :: pIk  = 589.65
      real(8), parameter :: p0   =  15.05 !-15.05

      real(8) npref
      real(8) ldocn,ldocd

! --- Organic and Inorganic Production Rate -----------------

      Pg= pmax*tanh(PFD/pIk)/3600   !Light response curve [mmolC/m2/s]
      R = p0/3600   !Light response curve [mmolC/m2/s]

      DICuptake= Pg-R
      DOuptake = R -Pg
      
! --- Nutrient fluxes between water column and coral -----
      npref= ninter(NH4amb)

      NO3uptake=Pg*nc*npref         !Flux caliculated in Carbon assimilated by Photosynthesis times C:N:P ratio [mmol/m2/s]
      NH4uptake=Pg*nc*(1.-npref)-R*nc
      PO4uptake=(Pg-R)*pc

      return
    end subroutine foodweb_seagrass


!!!*********************************************************************

    real(8) function ninter(NH4amb)
! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Caliculate ammnomiun inhibitation on nitrate uptake *
! *                                                                    *
! **********************************************************************
!
      implicit none
      real(8), intent(in) :: NH4amb

      ninter=0.15
      if(NH4amb.lt.0.1e0) ninter=0.9

      return
    end function ninter



#if defined TEST_GFW

!!!*********************************************************************


    subroutine gfw                &
!          input parameters
     &            (radi           &   ! Radiation (J/m2/sec) = (W/m2)
     &            ,Tamb           &   ! Temperature (oC)
     &            ,NO3amb         &   ! NO3 concentration (umol/L)
     &            ,NH4amb         &   ! NH4 concentration (umol/L)
     &            ,PO4amb         &   ! PO4 concentration (umol/L)
     &            ,LDOCamb        &   ! LDOC concentration (umol/L)
     &            ,RDOCamb        &   ! RDOC concentration (umol/L)
     &            ,POCamb         &   ! POC concentration (umol/L)
     &            ,CILamb         &   ! CIL concentration (cell/L)
     &            ,MPPamb         &   ! MPP concentration (umolC/L)
     &            ,MZPamb         &   ! MZP concentration (umolC/L)
!          output parameters
     &            ,DICuptake      &   ! DIC uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,TAuptake       &   ! TA  uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,DOuptake       &   ! DO  uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,NO3uptake      &   ! NO3 uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,NH4uptake      &   ! NH4 uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,PO4uptake      &   ! PO4 uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,LDOCuptake     &   ! LDOC uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,RDOCuptake     &   ! RDOC uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,POCuptake      &   ! POC uptake rate (umol L-1 s-1)  * direction of water column to organisms is positive
     &            ,CILuptake      &   ! Ciliate uptake rate (cell L-1 s-1)  * direction of water column to organisms is positive
     &            ,MPPuptake      &   ! Phytoplankton uptake rate (umolC L-1 s-1)  * direction of water column to organisms is positive
     &            ,MZPuptake      &   ! Zooplankton uptake rate (umolC L-1 s-1)  * direction of water column to organisms is positive
     &             )


! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Caliculate food grazing webs                        *
! *                                                                    *
! *                Notes:                                              *
! *                                                                    *
! *                1) Planktons are quantified by biomass (umolC/L)    *
! *                                                                    *
! *                2) Microbes are quantified by abundance (cell/L)    *
! *                                                                    *
! **********************************************************************
!
      implicit none
!
      real(8), intent(in) ::      &
     &             radi           &
     &            ,Tamb           &
     &            ,NO3amb         &
     &            ,NH4amb         &
     &            ,PO4amb         &
     &            ,RDOCamb        &
     &            ,CILamb         &
     &            ,MPPamb         &
     &            ,MZPamb          
! output parameters
      real(8), intent(out) ::     &
     &             DICuptake      &
     &            ,TAuptake       &
     &            ,DOuptake       &
     &            ,NO3uptake      &
     &            ,NO2uptake      &
     &            ,NH4uptake      &
     &            ,PO4uptake      &
     &            ,LDOCuptake     &
     &            ,RDOCuptake     &
     &            ,POCuptake      &
     &            ,MPPuptake      &
     &            ,MZPuptake       

!-----------------------------------------------------------------------
!l    1:Microphytoplankton [umolC/L]                                   l
!l    2:Mesozooplankton [umolC/L]                                      l
!l    4:Ciliate [cell/L]                                               l
!-----------------------------------------------------------------------
!
      real(8) gpp(1),rsp(3),mrt(3),!exc(2),grz(4),egs(2)
!
!      real(8) par,ldc,ssc,ssz(im,jm,kbm1),eirz(im,jm,kbm1)
!      real(8) rsp0(2),rtc(2)
!      real(8) mrt0(2),mtc(2)
!      real(8) ae(2),ge(2)
!      real(8) exex(1)
!      real(8) grmax(4),gtc(4),lvl(4),gtv(4)
!      real(8) rrn,rrp         !Redfield ratio
!      real(8) cnr(4),cpr(4)
      real(8) rno3(4)
!      real(8) nit,kn0,knt 
!      real(8) drdoc

      real(8), parameter :: rrn=106./16.
      real(8), parameter :: rrp=106./1.
      real(8), parameter :: cnr(1)=rrn
      real(8), parameter :: cnr(2)=rrn
      real(8), parameter :: cnr(4)=1./0.15
      real(8), parameter :: cpr(1)=rrp
      real(8), parameter :: cpr(2)=rrp
      real(8), parameter :: cpr(4)=1./(0.04*12./31.)

      real(8), parameter :: iopt(1)=0.00293/60.d0*1.d6 !!![J/m2/sec]
!      real(8), parameter :: par=0.43                  !Fraction of short wave available for photosynthesis [Anderson, 1993]
!      real(8), parameter :: ldc=0.035                 ![/m]
!      real(8), parameter :: ssc=0.0281/rrn            ![L/umolC/m]

      real(8), parameter :: vmax(1)=1.d0               ![/day]
      real(8), parameter :: kno3(1)=2.d0               ![umolN/L]
      real(8), parameter :: knh4(1)=0.6d0              ![umolN/L]
      real(8), parameter :: kpo4(1)=0.13d0             ![umolP/L]
      real(8), parameter :: aic(1)=1.5d0               !!![L/umolN]
      real(8), parameter :: kt(1)=0.03                 !!![/degC]
      real(8), parameter :: rsp0(1)=0.03               !!![/day]
      real(8), parameter :: rtc(1)=0.0519              ![/degC]
      real(8), parameter :: mrt0(1)=0.0585/cnr(1)      ![L/umolC/day]
      real(8), parameter :: mtc(1)=0.0693              ![/degC]
      real(8), parameter :: exex(1)=0.135

      real(8), parameter :: mrt0(2)=0.0585/cnr(2)      ![L/umolC/day]
      real(8), parameter :: mtc(2)=0.0693              ![/degC]
      real(8), parameter :: grmax(2)=0.3               ![/day]
      real(8), parameter :: gtc(2)=0.063               ![/degC]
      real(8), parameter :: lvl(2)=1.4/cnr(2)          ![L/umolC]
      real(8), parameter :: gtv(2)=0.01*cnr(2)         ![umolC/L]0.043
      real(8), parameter :: rsp0(2)=0.023              ![/day]
      real(8), parameter :: rtc(2)=0.0693              ![/degC]
      real(8), parameter :: ae(2)=0.7                  !
      real(8), parameter :: ge(2)=0.3                  !

      real(8), parameter :: grmax(4)=0.3               ![/day]
      real(8), parameter :: gtc(4)=0.063               ![/degC]
      real(8), parameter :: lvl(4)=1.4/cnr(4)          ![L/umolC]
      real(8), parameter :: gtv(4)=0.01*cnr(4)         ![umolC/L]

      real(8), parameter :: kn0=0.03                   ![/day]
      real(8), parameter :: knt=0.0693                 ![degC]

      real(8), parameter :: drdoc0=0.02/365.           ![/day]


!--- Phytoplankton ---
      gpp(1)=vmax(1) &
     &         *exp(kt(1)*Tamb)                                &
     &         *radi/iopt(1)*exp(1-radi/iopt(1)) &
     &         *dmin1(                                         &
     &                NO3amb/(NO3amb+kno3(1))   &
     &                 *exp(-aic(1)*NH4amb)     &
     &                 +NH4amb/(NH4amb+knh4(1)) &        !nitrogen limitation
     &               ,PO4amb/(PO4amb+kpo4(1))   &       !phosphatous limitation (H.Liu and B.Yin, 2010)
     &                )                         &
     &         *MPPamb                                     !primary production
     
      rsp(1)=rsp0(1)*exp(rtc(1)*Tamb))*MPPamb        !respiration
      
      rno3(1)=NO3amb/(NO3amb+kno3(1))     &
     &          *exp(-aic(1)*NH4amb)      &
     &          /(NO3amb/(NO3amb+kno3(1)) &
     &          *exp(-aic(1)*NH4amb)      &
     &          +NH4amb/(NH4amb+knh4(1)))
     
      mrt(1)=mrt0(1)*exp(mtc(1)*Tamb) *MPPamb**2      !mortality
      
      exc(1)=exex(1)*gpp(1)                           !extracellular excretion
      
!--- Meso-zooplankton ---
      mrt(2)=mrt0(2)*exp(mtc(2)*Tamb) *MZPamb**2
      
      grz(2)=dmax1(0.d0,                                   &
     &             grmax(2)*exp(gtc(2)*t(i,j,k))           &
     &             *(1.d0-exp(lvl(2)*(gtv(2)-(MPPamb       &
     &                          +CILamb*wpcc/12.*1.d6))))  &
     &             *MZPamb                                 &
     &             )                                        !grazing rate by mesozooplankton
     
      egs(2)=(1.d0-ae(2))*grz(2)                      !egestion
      
! use either excretion or respiration for zooplankton
      exc(2)=(ae(2)-ge(2))*grz(i,j,k,2)                     !excretion
      
      rsp(2)=rsp0(2)*exp(rtc(2)*Tamb)*MZPamb        !respiration
      
!--- Ciliate ---
      grz(4)=dmax1(0.d0,                                  &
     &             grmax(4)*exp(gtc(4)*Tamb)              &
     &             *(1.d0-exp(lvl(4)*(gtv(4)-MPPamb)))    &
!     &               +flg(i,j,k)*wpcf/12.*1.d6           &
!     &                +anf(i,j,k)*wpca/12.*1.d6))))      &
     &             *CILamb*wpcc/12.*1.d6                  &
     &             )                                        !grazing rate by ciliate
     
!--- Nitrofication ---
      nit=kn0*exp(knt*Tamb)*NH4amb                 !nitrification [umol/L/day]
      
!--- Decomposition of rDOC ---
      drdoc=drdoc0*RDOCamb                             !decomposition of rDOC to lDOC[umol/L/day]


!-----------Flux calculation (umol L-1 s-1) ----------------------------

      DICuptake=                  &    ! [umol kg-1 s-1]
     &       +(                   &
     &         -gpp(1) +rsp(1)    &
     &         +exc(2) +rsp(2)    &
     &         )/86400.d0

!      TAuptake=0.d0      ! [umol kg-1 s-1]

      NH4uptake=      ! [umol L-1 s-1]
     &       +(
     &         (-gpp(1)*(1.d0-rno3(1))
     &         +rsp(1))/cnr(1)
     &         +exc(2)/cnr(2)
     &         -nit
     &         )/86400.d0

      NO3uptake=      ! [umol L-1 s-1]
     &        +(
     &         -gpp(1)*rno3(1)/cnr(1)
     &         +nit
     &          )/86400.e0

!      NO2uptake=0.d0     ! [umol L-1 s-1]

      PO4uptake=
     &        +(
     &         (-gpp(1)+rsp(1))/cpr(1)
     &         +exc(2)/cpr(2)
     &          )/86400.d0
c
      LDOCuptake=      ! [umol L-1 s-1]
     &        +(
     &           exc(1)
     &          +drdoc
     &          )/86400.d0

      RDOCuptake=      ! [umol L-1 s-1]
     &         +(
     &          -drdoc
     &           )/86400.d0

      POCuptake=      ! [umol L-1 s-1]
     &         +(
     &           mrt(1) +mrt(2) +egs(2)
     &           )/86400.d0

!      LDONuptake=0.d0          ! [umol L-1 s-1]
!      RDONuptake=0.d0          ! [umol L-1 s-1]
!      PONuptake= 0.d0           ! [umol L-1 s-1]
!      
!      LDOPuptake=0.d0          ! [umol L-1 s-1]
!      RDOPuptake=0.d0          ! [umol L-1 s-1]
!      POPuptake= 0.d0           ! [umol L-1 s-1]

!      BACuptake= 0.d0             ! [cell L-1 s-1]

!      FLGuptake= 0.d0             ! [cell L-1 s-1]

      CILuptake=             ! [cell L-1 s-1]
     &        +(
     &          grz(4)
     &         -(grz(2)*(CILamb*wpcc/12.*1.d6)
     &           /(MPPamb+CILamb*wpcc/12.*1.d6))
     &          )/(wpcc/12.*1.d6)/86400.d0

!      ANFuptake=0.d0          ! [cell L-1 s-1]

      MPPuptake=      ! [umolC L-1 s-1]
     &        +(
     &          gpp(1) -rsp(1) -mrt(1)
     &         -exc(1)
     &         -grz(2)*MPPamb
     &           /(MPPamb+CILamb*wpcc/12.*1.d6)
     &         -grz(4)*MPPamb/MPPamb
!     &                        +anf(i,j,k)*wpca/12*1.d6)
     &           )/86400.d0

      MZPuptake=      ! [umolC L-1 s-1]
     &        +(
     &          grz(2) -mrt(2) -egs(2)
     &         -exc(2)
     &         -rsp(2)
     &          )/86400.d0

      return
    end subroutine gfw

#endif

!!!*********************************************************************

#if defined TEST_MFW

    subroutine mfw
! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Caliculate phytoplankton                            *
! *                                                                    *
! *                Notes:                                              *
! *                                                                    *
! *                Microbes are quantified by abundance (cell/L)       *
! *                                                                    *
! **********************************************************************

      implicit none

      include 'pom2k_v9.blk'

      integer i,j,k

!-----------------------------------------------------------------------
!l    1:Bacteria [cell/L]                                              l
!l    2:Cyanobacteria [cell/L]                                         l
!l    3:Autotrophic nanoflagellate [cell/L]                            l
!l    4,6:Heterotrophic nanoflagellate [cell/L]                          l
!l    5:Ciliate [cell/L]                                               l
!-----------------------------------------------------------------------

      double precision gpp(im,jm,kb,3),rsp(im,jm,kb,5),exc(im,jm,kb,5)
     &                ,grz(im,jm,kb,6),rno3(im,jm,kb,5),mrt(im,jm,kb,5)
     &                ,egs(im,jm,kb,5)
      double precision umax(6),mmk(6),tcg(6),cof1
      double precision vmax(3),exex(3),kno3(3),knh4(3),kpo4(3),aic(3)
     &                ,kt(3),mrt0(5),mtc(5),rsp0(5),rtc(5),ae(5),iopt(3)
      double precision cnr(5),cpr(5)
      double precision par,ldc,ssc,ssz(im,jm,kbm1),eirz(im,jm,kbm1)
      double precision bapp(im,jm,kb),bapr(im,jm,kb),bapd(im,jm,kb)
     &                ,vpd0,vpdt,exexd

      iopt(3)=0.00293/60.d0*1.d6 ![J/m2/sec]
      par=0.43                   !Fraction of short wave available for photosynthesis [Anderson, 1993]
      ldc=0.035                  ![/m]
      ssc=0.0281/106./16.        ![L/umolC/m]

      cnr(1)=1./(0.22*12./14.)
      cpr(1)=1./(0.04*12./31.)   !(Vrede, 1998)
      cnr(3)=1./0.15
      cpr(3)=1./(0.04*12./31.)
      cnr(4)=1./0.15
      cpr(4)=1./(0.04*12./31.)   !(Y.Sin, 1998 for C:N)
      cnr(5)=1./0.15
      cpr(5)=1./(0.04*12./31.)

      cof1=0.36                  !fraction of N or P uptake from DIN(DIP) to DON(DOP) (pp.138)
      kno3(1)=2.d0               ![umolN/L]
      knh4(1)=0.6d0              ![umolN/L]
      aic(1)=1.5d0               ![L/umolN]
      exex(1)=0.135

      vmax(3)=1.d0               ![/day]
      kno3(3)=2.d0               ![umolN/L]
      knh4(3)=0.6d0              ![umolN/L]
      kpo4(3)=0.13d0             ![umolP/L]
      aic(3)=1.5d0               ![L/umolN]
      kt(3)=0.03                ![/degC]
      rsp0(3)=0.03               ![/day]
      rtc(3)=0.0519              ![/degC]
      mrt0(3)=0.0585/cnr(3)*wpca/12*1.d6![L/cell/day]
      mtc(3)=0.0693              ![/degC]
      exex(3)=0.135

      mrt0(4)=0.0585*wpcf/12*1.d6/cnr(4)![L/cell/day]
      mtc(4)=0.0693              ![/degC]
      rsp0(4)=0.064              ![/day]
      rtc(4)=0.0693              ![/degC]
      ae(4)=0.7                  !Egestion ratio to grazing
      umax(4)=60.d0              !maximum consumption rate of 60 bacteria per hour(Fenchel,1982)
      tcg(4)=0.34d9              !threshold concentration for growth (Boenigk etal.,2006=0.34d9  Fenchel,1982=0.d0)
      mmk(4)=1.18d9              !half saturation constant for a Michaelis-Menten type equation (Boenigk etal.,2006=1.18d9  Fenchel,1982=5.d9)
      umax(6)=1.d-2               !maximum consumption rate of ANF per hour
      tcg(6)=0.1d6               !threshold concentration for growth
      mmk(6)=0.8d6               !half saturation constant for a Michaelis-Menten type equation

      mrt0(5)=0.0585*wpcf/12*1.d6/cnr(5)![L/cell/day]
      mtc(5)=0.0693              ![/degC]
      rsp0(5)=0.064              ![/day]
      rtc(5)=0.0693              ![/degC]
      ae(5)=0.7                  !Egestion ratio to grazing
      umax(5)=12.d0              !maximum consumption rate of ANF and HNF per hour
      tcg(5)=0.8d6               !threshold concentration for growth
      mmk(5)=3.0d6               !half saturation constant for a Michaelis-Menten type equation

      vpd0=0.03                  ![/day]
      vpdt=0.0693                ![/degC]
      exexd=0.135                !Extracellular excretion due to POC decomposition

!$OMP parallel
!$OMP do
      do i=1,im
        do j=1,jm
        
          eirr=(-radi(i,j,k)+radir(i,j,k+1)) !TN:add. solar radiation (W m-2)

          ssz(i,j,1)=(mph(i,j,1)+mzh(i,j,1))*dt(i,j)*abs(zz(1))
          eirz(i,j,1)=(1.d0-0.06d0)*par*eirr                            !albedo=0.06
     &                  *exp(-(ldc*dt(i,j)*abs(zz(1))+ssc*ssz(i,j,1)))
          do k=2,kbm1
            ssz(i,j,k)=ssz(i,j,k-1)
     &                  +0.5d0*(mph(i,j,k-1)+mzh(i,j,k-1)
     &                         +mph(i,j,k)+mzh(i,j,k))
     &                  *dt(i,j)*dzz(k-1)
            eirz(i,j,k)=(1.d0-0.06d0)*par*eirr
     &                    *exp(-(ldc*dt(i,j)*abs(zz(k))+ssc*ssz(i,j,k)))
          enddo
        enddo
      enddo
!$OMP end do
c
!$OMP do
      do i=1,im
        do j=1,jm
          do k=1,kbm1
c--- Bacteria ----
c*** DOC reminelarization ***
c            bgr(i,j,k)=(0.03018*(ldocf(i,j,k)/1000.)**0.576155)          !Bacteria Growth Rate(Specific Growth Rate(Eiler etal,2003)
            bgr(i,j,k)=0.34/(1+exp(-0.19*(ldocf(i,j,k)-23.)))-4.247d-3  !Bacteria Growth Rate(refrence from Nakajima etal. 2009)
            gpp(i,j,k,1)=bctf(i,j,k)*(exp(bgr(i,j,k))-1)                !NET Bacteria Production Rate [cell/L/h] <-Gross - Respiration
            bge(i,j,k)=0.222894
     &                  *(gpp(i,j,k,1)*wpcb/12*1.d6)                    !convert to umolC per L [umolC/L/h]
     &                  +0.068929                                       !Bacterial Growth Efficiency(Pradeep and chandramohan,2003)
            rsp(i,j,k,1)=(1/bge(i,j,k)-1.)*gpp(i,j,k,1)*wpcb/12*1.d6    !Bacteria Respiration [umolC/L/h]
            gpp(i,j,k,1)=gpp(i,j,k,1)/3600.d0                           ![cell/L/sec]
            rsp(i,j,k,1)=rsp(i,j,k,1)/3600.d0                           ![umolC/L/sec]
            exc(i,j,k,1)=gpp(i,j,k,1)*exex(1)
c            exc(i,j,k,1)=(gpp(i,j,k,1)+rsp(i,j,k,1)/(wpcb/12*1.d6))
c     &                     *exex(1)
            rno3(i,j,k,1)=no3(i,j,k)/(no3(i,j,k)+kno3(1))
     &                      *exp(-aic(1)*nh4(i,j,k))
     &                      /(no3(i,j,k)/(no3(i,j,k)+kno3(1))
     &                      *exp(-aic(1)*nh4(i,j,k))
     &                      +nh4(i,j,k)/(nh4(i,j,k)+knh4(1)))
            fnh4b(i,j,k)=rsp(i,j,k,1)/cnr(1)                            !Bacterial Nitrogen release by respiration [umolN/L/sec]
     &                   -(gpp(i,j,k,1)*wpcb/12*1.d6+rsp(i,j,k,1))
     &                     /cnr(1)/(1.d0+cof1)*(1.d0-rno3(i,j,k,1))            !Bacterial Nitrogen uptake from NH4
            fno3b(i,j,k)=-(gpp(i,j,k,1)*wpcb/12*1.d6+rsp(i,j,k,1))
     &                     /cnr(1)/(1.d0+cof1)*rno3(i,j,k,1)            !Bacterial Nitrogen uptake from NO3
            fpo4b(i,j,k)=rsp(i,j,k,1)/cpr(1)                            !Bacterial Phosphorus release by respiration [umolP/L/sec]
     &                   -(gpp(i,j,k,1)*wpcb/12*1.d6+rsp(i,j,k,1))
     &                     /cpr(1)/(1.d0+cof1)                          !Bacterial Phosphorus uptake from PO4
c-- Bacteria Attached to Particle(BAP) ---
            bapd(i,j,k)=vpd0*exp(vpdt*t(i,j,k))*lpoc(i,j,k)/86400.d0    !Bacterial decomposition rate of POC into DOC [umolC/L/sec]
            bapp(i,j,k)=                                                !BAP production rate[umolC/L/sec]
     &                  bapd(i,j,k)
     &                  /exexd                                          !Bacterial gross production by POC decomposition(=production+respiration)
     &                  *(0.374-0.0104*t(i,j,k))                        !Bacterial Growth Efficiency
c            bapr(i,j,k)=3.69*bapp(i,j,k)**0.58                          !BAP respiration rate(del Giorgio and Cole, 1998) [umolC/L/sec]
            bapr(i,j,k)=bapd(i,j,k)/exexd*(1.-(0.374-0.0104*t(i,j,k)))
c--- Autotrophic Nano Flagellate ---
            gpp(i,j,k,3)=vmax(3)
     &                  *exp(kt(3)*t(i,j,k))
     &                  *eirz(i,j,k)/iopt(3)*exp(1-eirz(i,j,k)/iopt(3))
     &                  *dmin1(
     &                         (no3(i,j,k)/(no3(i,j,k)+kno3(3))
     &                           *exp(-aic(3)*nh4(i,j,k))
     &                           +nh4(i,j,k)/(nh4(i,j,k)+knh4(3)))      !nitrogen limitation
     &                         ,po4(i,j,k)/(po4(i,j,k)+kpo4(3))         !phosphatous limitation (H.Liu and B.Yin, 2010)
     &                         )
     &                  *anf(i,j,k)                                     !primary production [cell/L/day]
            rsp(i,j,k,3)=rsp0(3)*exp(rtc(3)*t(i,j,k))*anf(i,j,k)        !equivalent cell decrease to respiration [cell/L/day]
            rno3(i,j,k,3)=no3(i,j,k)/(no3(i,j,k)+kno3(3))
     &                      *exp(-aic(3)*nh4(i,j,k))
     &                      /(no3(i,j,k)/(no3(i,j,k)+kno3(3))
     &                      *exp(-aic(3)*nh4(i,j,k))
     &                      +nh4(i,j,k)/(nh4(i,j,k)+knh4(3)))
            mrt(i,j,k,3)=mrt0(3)*exp(mtc(3)*t(i,j,k))
     &                    *anf(i,j,k)**2                                !equivalent cell decrease to mortality [cell/L/day]
            exc(i,j,k,3)=exex(3)*gpp(i,j,k,3)                           !equivalent cell decrease to extracellular excretion [cell/L/day]
c--- Heterotrophic Nano Flagellate ---
            grz(i,j,k,4)=umax(4)*(bct(i,j,k)-tcg(4))
     &                   /((bct(i,j,k)-tcg(4))+mmk(4))
     &                   *flg(i,j,k)/3600.d0                            !Bacteria grazing rate by flagellate[bacteria cell/L/sec](Fenchel,1982)
            grz(i,j,k,6)=umax(6)*(anf(i,j,k)-tcg(6))
     &                   /((anf(i,j,k)-tcg(6))+mmk(6))
     &                   *flg(i,j,k)/3600.d0                            !ANF grazing rate by flagellate[bacteria cell/L/sec](Fenchel,1982)
            mrt(i,j,k,4)=mrt0(4)*exp(mtc(4)*t(i,j,k))
     &                    *flg(i,j,k)**2/86400.d0                       !HNF mortality [HNF cell/L/sec]
            rsp(i,j,k,4)=rsp0(4)*exp(rtc(4)*t(i,j,k))*flg(i,j,k)/86400. !HNF respiration [HNF cell/L/sec]
            egs(i,j,k,4)=(1.d0-ae(4))*(grz(i,j,k,4)*wpcb/wpcf
     &                                +grz(i,j,k,6)*wpca/wpcf)          !HNF egestion rate [HNF cell/L/sec]
c            fre(i,j,k)=(1./0.01-1.)*fpr(i,j,k)*wpcf/12*1.d6              !Flagellate Respiration [umolC/L/sec]
c            fnh4f(i,j,k)=fre(i,j,k)/cnr(4)                                 !Flagellate Nitrogen release by respiration [umolN/L/sec]
c            fpo4f(i,j,k)=fre(i,j,k)/cpr(4)                                 !Flagellate Phosphorus release by respiration [umolP/L/sec]
c--- Ciliate ---
            grz(i,j,k,5)=umax(5)*(flg(i,j,k)+anf(i,j,k)-tcg(5))
     &                     /((flg(i,j,k)+anf(i,j,k)-tcg(5))+mmk(5))
     &                     *cil(i,j,k)/3600.d0                          !Flagellate grazing rate by ciliate[flagellate cell/L/sec]
            mrt(i,j,k,5)=mrt0(5)*exp(mtc(5)*t(i,j,k))
     &                     *cil(i,j,k)**2/86400.d0                      !Ciliate mortality [Ciliate cell/L/sec]
            rsp(i,j,k,5)=rsp0(5)*exp(rtc(5)*t(i,j,k))*cil(i,j,k)/86400. !Ciliate respiration [Ciliate cell/L/sec]
            egs(i,j,k,5)=(1.d0-ae(5))
     &                     *(grz(i,j,k,5)*.5d0*(wpcf+wpca)/wpcc)        !Ciliate egestion rate [ciliate cell/L/sec]
          enddo
        enddo
      enddo
!$OMP end do
c
      do j=2,jmm1
        do i=2,imm1
          if(fsm(i,j).eq.1.) then
            do k=1,kbm1

              R_wc(nDIC,i,j,k)=R_wc(nDIC,i,j,k)      ! [umol L-1 s-1]
     &                +rsp(i,j,k,1)
     &                +(-gpp(i,j,k,3)+rsp(i,j,k,3))*wpca/12*1.d6/86400.
     &                +rsp(i,j,k,4)*wpcf/12*1.d6
     &                +rsp(i,j,k,5)*wpcc/12*1.d6
     &                +bapr(i,j,k)

              R_wc(nTA, i,j,k)=R_wc(nTA, i,j,k)      ! [umol L-1 s-1]
c
              R_wc(nNH4,i,j,k)=R_wc(nNH4,i,j,k)      ! [umol L-1 s-1]
     &                +fnh4b(i,j,k)
     &                +(-gpp(i,j,k,3)*(1.e0-rno3(i,j,k,3))+rsp(i,j,k,3))
     &                  *wpca/12*1.d6/cnr(3)/86400.d0
     &                +rsp(i,j,k,4)*wpcf/12*1.d6/cnr(4)
     &                +rsp(i,j,k,5)*wpcc/12*1.d6/cnr(5)
     &                +bapr(i,j,k)/cnr(1)

              R_wc(nNO3,i,j,k)=R_wc(nNO3,i,j,k)      ! [umol L-1 s-1]
     &                +fno3b(i,j,k)
     &                -gpp(i,j,k,3)*rno3(i,j,k,3)
     &                  *wpca/12*1.d6/cnr(3)/86400.d0

              R_wc(nNO2,i,j,k)=R_wc(nNO2,i,j,k)-0.d0 ! [umol L-1 s-1]

              R_wc(nPO4,i,j,k)=R_wc(nPO4,i,j,k)
     &                +fpo4b(i,j,k)
     &                +(-gpp(i,j,k,3)+rsp(i,j,k,3))*wpca/12*1.d6/cpr(3)
     &                  /86400.d0
     &                +rsp(i,j,k,4)*wpcf/12*1.d6/cpr(4)
     &                +rsp(i,j,k,5)*wpcc/12*1.d6/cpr(5)
     &                +bapr(i,j,k)/cnr(1)
c
              R_wc(nLDOC,i,j,k)=R_wc(nLDOC,i,j,k)      ! [umol L-1 s-1]
     &                 -(gpp(i,j,k,1)*wpcb/12*1.d6+rsp(i,j,k,1))
c     &                 +exc(i,j,k,1)*wpcb/12*1.d6
     &                 +exc(i,j,k,3)*wpca/12*1.d6/86400.d0
c     &                 +bapd(i,j,k)

              R_wc(nRDOC,i,j,k)=R_wc(nRDOC,i,j,k)      ! [umol L-1 s-1]
     &                 +exc(i,j,k,1)*wpcb/12*1.d6
     &                 +bapd(i,j,k)

              R_wc(nPOC ,i,j,k)=R_wc(nPOC ,i,j,k)      ! [umol L-1 s-1]
     &                 +mrt(i,j,k,3)*wpca/12*1.d6/86400.d0
     &                 +(mrt(i,j,k,4) +egs(i,j,k,4))*wpcf/12*1.d6
     &                 +(mrt(i,j,k,5) +egs(i,j,k,5))*wpcc/12*1.d6
     &                 -bapd(i,j,k)/exexd
c
              R_wc(nLDON,i,j,k)=R_wc(nLDON,i,j,k)            ! [umol L-1 s-1]
              R_wc(nRDON,i,j,k)=R_wc(nRDON,i,j,k)            ! [umol L-1 s-1]
              R_wc(nPON ,i,j,k)=R_wc(nPON ,i,j,k)            ! [umol L-1 s-1]
              
              R_wc(nLDOP,i,j,k)=R_wc(nLDOP,i,j,k)            ! [umol L-1 s-1]
              R_wc(nRDOP,i,j,k)=R_wc(nRDOP,i,j,k)            ! [umol L-1 s-1]
              R_wc(nPOP ,i,j,k)=R_wc(nPOP ,i,j,k)            ! [umol L-1 s-1]
c
              R_wc(nBAC,i,j,k)=R_wc(nBAC,i,j,k)             ! [cell L-1 s-1]
     &                +gpp(i,j,k,1) -exc(i,j,k,1) -grz(i,j,k,4)

              R_wc(nFLG,i,j,k)=R_wc(nFLG,i,j,k)             ! [cell L-1 s-1]
     &                +grz(i,j,k,4)*wpcb/wpcf                           !Flagellate production Rate [cell/L/sec]
     &                +grz(i,j,k,6)*wpca/wpcf
     &                -egs(i,j,k,4) -mrt(i,j,k,4) -rsp(i,j,k,4)
     &                -grz(i,j,k,5)*flg(i,j,k)/(flg(i,j,k)+anf(i,j,k))

              R_wc(nCIL,i,j,k)=R_wc(nCIL,i,j,k)             ! [cell L-1 s-1]
     &                +grz(i,j,k,5)*.5d0*(wpcf+wpca)/wpcc
     &                -egs(i,j,k,5) -mrt(i,j,k,5) -rsp(i,j,k,5)

              R_wc(nANF,i,j,k)=R_wc(nANF,i,j,k)             ! [cell L-1 s-1]
     &               +(gpp(i,j,k,3) -rsp(i,j,k,3) -mrt(i,j,k,3)
     &                -exc(i,j,k,3))/86400.d0
     &                -grz(i,j,k,6)
     &                -grz(i,j,k,5)*anf(i,j,k)/(flg(i,j,k)+anf(i,j,k))
              R_wc(nMPH,i,j,k)=R_wc(nMPH,i,j,k)      ! [umolC L-1 s-1]
              R_wc(nMZH,i,j,k)=R_wc(nMZH,i,j,k)      ! [umolC L-1 s-1]
c
              R_wc(nDI13C,i,j,k)=R_wc(nDI13C,i,j,k)          ! [umol L-1 s-1]


            end do
          endif
        end do
      end do

      return
    end subroutine mfw
    
#endif
!!!*********************************************************************

  end module mod_foodweb


