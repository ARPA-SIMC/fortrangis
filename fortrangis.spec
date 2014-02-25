Summary: FortranGIS Fortran interfaces Open Source GIS libraries 
Name: fortrangis
Version: 2.3
Release: 1
License: LGPL
Group: Applications/GIS
URL: http://fortrangis.berlios.de/
Packager: Davide Cesari <dcesari69@gmail.com>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot
BuildRequires: shapelib-devel gdal-devel proj-devel

%if 0%{?fedora} < 9
%define _fmoddir       %{_libdir}/gfortran/modules
%endif

%package -n fortrangis-devel
Summary:  FortranGIS development files
Group: Applications/GIS


%package -n fortrangis-doc
Summary:  FortranGIS documentation
Group: Applications/GIS

%description -n fortrangis-devel
Development files, necessary for building applications using
FortranGIS

%description -n fortrangis-doc
Doxygen documentation for FortranGIS package

%description
FortranGIS is a collection of Fortran interfaces to the most common
Open Source GIS libraries, plus some more Fortran-specific tools.

The libraries interfaced at the moment are Shapelib, GDAL and Proj.

%prep
%setup -q

%build
%configure CPPFLAGS=-I/usr/include/libshp
make 

%install
make DESTDIR=%{buildroot} install
%if 0%{?fedora} >= 9
mkdir -p $RPM_BUILD_ROOT%{_fmoddir}
mv $RPM_BUILD_ROOT%{_includedir}/*.mod $RPM_BUILD_ROOT%{_fmoddir}
%endif

%files
%defattr(-, root, root)
%{_libdir}/*.so.*
%doc %{_datadir}/doc/%{name}-%{version}/COPYING
%doc %{_datadir}/doc/%{name}-%{version}/README

%files -n fortrangis-devel
%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so
%if 0%{?fedora} < 9
%{_includedir}/*
%else
%{_fmoddir}/*.mod
%endif

%files -n fortrangis-doc
%defattr(-,root,root,-)
%doc %{_datadir}/doc/%{name}-%{version}/html

%clean
rm -rf %{buildroot}

%pre

%post

%preun

%postun

%changelog
