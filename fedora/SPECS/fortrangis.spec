Summary: FortranGIS Fortran interfaces Open Source GIS libraries 
Name: fortrangis
Version: 2.4
Release: 2
License: LGPL
Group: Applications/GIS
URL: http://fortrangis.berlios.de/
Packager: Davide Cesari <dcesari69@gmail.com>
#Source: %{name}-%{version}.tar.gz
Source: https://github.com/arpa-simc/%{name}/archive/v%{version}-%{release}.tar.gz#/%{name}-%{version}-%{release}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildRequires: shapelib-devel gdal-devel proj-devel

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
%setup -q -n %{name}-%{version}-%{release}

%build

autoreconf -ifv

%configure CPPFLAGS=-I/usr/include/libshp
make 

%install
[ "%{buildroot}" != / ] && rm -rf "%{buildroot}"
make DESTDIR=%{buildroot} install

%clean
[ "%{buildroot}" != / ] && rm -rf "%{buildroot}"

%files
%defattr(-, root, root)
%{_libdir}/*.so.*
%doc %{_datadir}/doc/%{name}-%{version}/COPYING
%doc %{_datadir}/doc/%{name}-%{version}/README

%files -n fortrangis-devel
%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so
%{_fmoddir}/*.mod

%files -n fortrangis-doc
%defattr(-,root,root,-)
%doc %{_datadir}/doc/%{name}-%{version}/html

%pre

%post

%preun

%postun

%changelog
* Tue Jul 12 2016 Daniele Branchini <dbranchini@arpae.it> - 2.4-2
- automatizing rpmbuild
