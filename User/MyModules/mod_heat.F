
!!!====== ver 2013/01/10 ===============================================

#include "cppdefs.h"


!!!**** Heat MODULE ************************************

  module mod_heat

    implicit none

    real(8), allocatable, save ::                 &
     &  radi(:,:,:)    ,radir(:,:,:)            

  contains

!!! **********************************************************************
!!!  set initial conditions for ecosystem model
!!! **********************************************************************

      subroutine heat_setini(im,jm,kb)

      implicit none
      
      integer, intent(in) :: im,jm,kb  !Grid size of x, y, z
      
!      integer, parameter :: imm1=im-1, jmm1=jm-1, kbm1=kb-1

      integer i,j,k

      allocate( radi(im,jm,kb)   ,radir(im,jm,kb) )

!   Initialization
      radi(:,:,:)=0.
      radir(:,:,:)=0.

      return
    end subroutine heat_setini


! **********************************************************************
!  shortwave radiation (W/m2) of water column
! **********************************************************************

    subroutine shortwave(im,jm,kb,ssradi,dz,d)
!
      implicit none

      integer, intent(in) :: im,jm,kb  !Grid size of x, y, z
      real(8), intent(in) :: ssradi    ! Sea surface solar radiation (W m-2)
      real(8), intent(in) :: dz(kb)    ! ohmega grid size
      real(8), intent(in) :: d(im,jm)  ! water depth (m)

      real(8) tr, bsed,cbsed
        !Tr: transmission, 
        !bsed: turbidity parameter; offshere water = 1, 
        !                           coast water    = 2-3, 
        !                           cloudy water   = 5-9
        !cbsed:adjusting parameter for bsed
      integer i,j,k

! Ref. Kondo et al.(1979) J. Phys. Oceanogr. 9, 360-372.
!      Kondo (1994) book

      cbsed=10. !!!“K“–!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Check after Sed changed

      do j=2,jm-1
        do i=2,im-1
        
!         Sea surface
          radi(i,j,1)=(1.e0-0.07)*ssradi
                  
          do k=1,kb-1
            bsed=1.0e0!+cbsed*Ci(nSed,i,j,1)
            tr= 0.14*exp(500.0e0 *dz(k)*d(i,j))         &
     &         +0.23*exp( 12.0e0 *dz(k)*d(i,j))         &
     &         +0.14*exp( 25.0e0 *dz(k)*d(i,j))         &
     &         +0.13*exp(  0.4e0 *dz(k)*d(i,j))         &
     &         +0.20*exp(  0.1e0 *dz(k)*d(i,j)*bsed)         &
     &         +0.16*exp(  0.04e0*dz(k)*d(i,j)*bsed)
            radi(i,j,k+1)=radi(i,j,1)*tr
            
          end do

!         Refrection from bottom
          radir(i,j,kb)=-(1.e0-0.7)*radi(i,j,kb) !TN:Albedo of bottom sediment = 0.7            
          
          do k=kb-1,1,-1
            bsed=1.0e0!+cbsed*Ci(nSed,i,j,1)
            tr= 0.14*exp(500.0e0 *dz(k)*d(i,j))         &
     &         +0.23*exp( 12.0e0 *dz(k)*d(i,j))         &
     &         +0.14*exp( 25.0e0 *dz(k)*d(i,j))         &
     &         +0.13*exp(  0.4e0 *dz(k)*d(i,j))         &
     &         +0.20*exp(  0.1e0 *dz(k)*d(i,j)*bsed)         &
     &         +0.16*exp(  0.04e0*dz(k)*d(i,j)*bsed)
            radir(i,j,k)=radir(i,j,k+1)*tr
          end do
          
        end do
      end do
      
      return
    end subroutine shortwave


!! **********************************************************************
!!  Caliculate heat flux between water column and soil. 
!! **********************************************************************
!
!    subroutine calgsoil
!!
!      implicit none
!
!
!      dzzzz = 0.03                        !  [m]
!      dzzzz2 = dzzzz*dzzzz
!      ag = 5.9d-7                      !  [m2/s]
!      cgrg =3.0d+6                     !  [J/m3/K]
!
!!$OMP parallel
!!$OMP do
!      do j=1,jm
!        do i=1,im
!
!          Tg(i,j,1)=t(i,j,kbm1)
!
!          do loop=1,20
!
!            do k=2,kmaxG-1
!              Tgn(i,j,k)=(Tgb(i,j,k)
!     &            +dti*ag*(Tg(i,j,k+1)+Tg(i,j,k-1))/dzzzz2)         &
!     &                                   /(1.0+2.*ag*dti/dzzzz2)
!            enddo
!
!            error = 0.e0
!            do k=2,kmaxG-1
!              error = max( error,abs(Tgn(i,j,k)-Tg(i,j,k)) )
!            enddo
!
!            if(error.le.1.0d-6) goto 1000
!
!            do k=2,kmaxG-1
!              Tg(i,j,k) = Tgn(i,j,k)
!            enddo
!
!!:      b.c for Tg
!
!            Tg(i,j,1)    = t(i,j,kbm1)
!            Tg(i,j,kmaxG)=Tg(i,j,kmaxG-1)
!
!          enddo
!
! 1000   continue
!
!          Gsoil(i,j) = 0.e0
!          Gsoil(i,j) = Gsoil(i,j)         &
!     &                 + cgrg*(Tg(i,j,2)-Tgb(i,j,2))/dti*dzzzz
!          do k=2,kmaxG
!            Gsoil(i,j) = Gsoil(i,j)         &
!     &                 + cgrg*(Tg(i,j,k)-Tgb(i,j,k))/dti*dzzzz   ! [J/m2/s]
!          enddo
!
!          Gsoil(i,j)=Gsoil(i,j)/rhoref         &                           ! [m*K/s]
!     &             /(3.958-0.0523*s(i,j,kbm1)+0.000837*t(i,j,kbm1))/1000  !Bromley, L.A. etal,(1967)
!
!          if(iint.gt.int(3600.e0/dti)) then
!            Gcoef = 1.0
!          else
!            Gcoef = 0.5*(1.-cos(pi*(iint*dti)/3600.0))
!          endif
!
!          Gsoil(i,j) = Gcoef * Gsoil(i,j)
!
!        enddo
!      enddo
!!$OMP end do
!
!!$OMP do
!      do j=1,jm
!        do i=1,im
!          do k=1,kmaxG
!            Tgb(i,j,k)=Tg(i,j,k)
!         enddo
!        enddo
!      enddo
!!$OMP end do
!!$OMP end parallel
!
!      return
!    end subroutine calgsoil


! **********************************************************************
!  Evaporation volume flux (m s-1)
! **********************************************************************

    real(8) function evaporation(tsurf, Ssurf, tair, pair, eair, u10) ! Evaporation volume flux (m s-1)

      implicit none
!                          input parameters
      real(8), intent(in) :: tsurf   ! Sea surface temperature (oC)
      real(8), intent(in) :: Ssurf   ! Sea surface salinity (psu)
      real(8), intent(in) :: tair    ! air temperature (oC)
      real(8), intent(in) :: pair    ! atm pressure (hPa)
      real(8), intent(in) :: eair    ! vapor pressur (hPa)
      real(8), intent(in) :: u10     ! wind speed (m s-1)
!      real(8), intent(in) :: Hum     !relative humidity (%)

      real(8) rhoatm, esw, spHumw, spHuma   

!      tair=27.0
!      pair=1013.0
!      Hum=50.e0   ! Humidity

!      eair=sat_vapor_press(tair)*Hum/100.e0    ! vapor pressur (hPa)
      rhoatm=dens_air(tair,pair,eair)                   !density of air (kg m-3)
      spHuma=specific_humidity(eair,pair)               !air specific humidity (kg kg-1)
      esw=(1.e0-5.37e-4*Ssurf)*sat_vapor_press(tsurf)   !vapor pressur on sea surface(hPa)
      spHumw=specific_humidity(esw,pair)                !specific humidity at sea surface (kg kg-1)

      evaporation=rhoatm*1.2e-3*u10*(spHumw-spHuma)*1.0e-3 ! Evaporation volume flux (m s-1)

      return
    end function evaporation


! **********************************************************************
!  Sensible heat (W m-2)
! **********************************************************************

    real(8) function sensheat(tsurf, tair, pair, eair, u10) !sensible heat (W m-2)

      implicit none
      
      real(8), intent(in) :: tsurf   ! Sea surface temperature (oC)
      real(8), intent(in) :: tair    ! air temperature (oC)
      real(8), intent(in) :: pair    ! atm pressure (hPa)
      real(8), intent(in) :: eair    ! vapor pressur (hPa)
      real(8), intent(in) :: u10     ! wind speed (m s-1)
!      real(8), intent(in) :: Hum     !relative humidity (%)

      real(8) rhoatm   !density of air (kg m-3)

!      tair=27.0
!      pair=1013.0
!      Hum=50.e0   ! Humidity
!      eair=sat_vapor_press(tair)*Hum/100.e0    ! vapor pressur (hPa)

      rhoatm=dens_air(tair,pair,eair)                 !density of air (kg m-3)
      sensheat=1005.e0*rhoatm*1.2e-3*u10*(tsurf-tair) !sensible heat (W m-2)
              !1005 J kg-1 K-1 -> ‹ó‹C‚Ì’èˆ³”ä”M
      return
    end function sensheat

! **********************************************************************
!  Long wave radiation (W m-2) from sea to atmosphere
! **********************************************************************

    real(8) function long_wave_radi(tsurf) ! Long wave radiation (W m-2)

      implicit none
!                          input parameters
      real(8), intent(in) :: tsurf   ! Sea surface temperature (oC)
      
      long_wave_radi = 0.96*(5.670e-8*(tsurf+273.15)**4-295.0)!295.“K“– ‘å‹C¨ŠC—m‚Ö‚Ì’·”g•úË(W/m2)
      
      return
    end function long_wave_radi


! **********************************************************************
!  density of air (kg m-3)
! **********************************************************************

    real(8) function dens_air(tair,pair,eair)!  density of air (kg m-3)

      implicit none

      real(8), intent(in) :: tair    ! air temperature (oC)
      real(8), intent(in) :: pair    ! atm pressure (hPa)
      real(8), intent(in) :: eair    ! vapor pressur (hPa)


      dens_air=1.293*273.15/(273.15+tair)*(pair/1013.25)  &
     &          *(1-0.378*eair/pair)

      return
    end function dens_air


! **********************************************************************
!  Saturation vapor pressure (hPa)
! **********************************************************************

    real(8) function sat_vapor_press(tair)

      implicit none

      real(8), intent(in) :: tair    ! air temperature (oC)

      sat_vapor_press=6.1078*10.**(7.5*tair/(237.3+tair))

      return
    end function sat_vapor_press
      
      
! **********************************************************************
!  Specific humidity (kg kg-1)
! **********************************************************************

    real(8) function specific_humidity(eair,pair)

      implicit none
      
      real(8), intent(in) :: pair    ! atm pressure (hPa)
      real(8), intent(in) :: eair    ! vapor pressur (hPa)
      
      real(8) e_per_p

      e_per_p=eair/pair
      specific_humidity=0.622*e_per_p/(1.-0.378*e_per_p)

      return
    end function specific_humidity
    
    
  end module mod_heat


