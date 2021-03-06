FROM base/archlinux

RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm numactl git autoconf automake libtool clang llvm make unzip gcc-objc clamav pkg-config openssl-1.0 libgsf intltool gcab vala gobject-introspection bison p7zip

# set locale to avoid problems with char conversions
RUN /bin/echo -e 'LANG=en_US.UTF-8\n' > /etc/locale.conf && \
	/bin/echo -e 'en_US.UTF-8 UTF-8\n' >> /etc/locale.gen && \
	locale-gen

# build cctools
RUN git clone --depth 1 https://github.com/tpoechtrager/cctools-port /root/cctools \
	&& cd /root/cctools/cctools \
	&& autoreconf --install --force \
	&& mkdir -p /usr/local/cctools \
	&& CC=clang CXX=clang++ ./configure --prefix=/usr/local/cctools \
	&& make -j$(nproc) \
	&& make install \
	&& rm -rf /root/cctools

# build gnustep-make
RUN git clone --depth 1 https://github.com/gnustep/tools-make /root/gnustep-make \
	&& cd /root/gnustep-make \
	&& autoreconf --install --force \
	&& ./configure \
	&& make -j$(nproc) \
	&& make install \
	&& rm -rf /root/gnustep-make

# build gnustep-base
RUN git clone --depth 1 https://github.com/gnustep/libs-base /root/gnustep-base \
	&& cd /root/gnustep-base \
	&& source /usr/local/share/GNUstep/Makefiles/GNUstep.sh \
	&& autoreconf --install --force \
	&& ./configure \
	&& make -j$(nproc) \
	&& make install \
	&& rm -rf /root/gnustep-base

# build unar
RUN git clone --depth 1 -b unar-1.10.1 https://github.com/incbee/Unarchiver.git /root/unar \
	&& cd /root/unar/XADMaster \
	&& make -j$(nproc) -f Makefile.linux \
	&& mv unar /usr/bin/ \
	&& rm -rf /root/unar

# build osslsigncode
RUN git clone https://git.code.sf.net/p/osslsigncode/osslsigncode /root/osslsigncode \
	&& cd /root/osslsigncode \
	&& git checkout a912601140ed030eaa61f681f057175cda2cc2de \
	&& autoreconf --install --force \
	&& PKG_CONFIG_PATH=/usr/lib/openssl-1.0/pkgconfig ./configure --prefix=/usr/local --bindir=/usr/local/bin --sbindir=/usr/local/bin \
	&& make -j$(nproc) \
	&& make install \
	&& rm -rf /root/osslsigncode

# build msitools
RUN git clone --depth 1 -b v0.97 https://github.com/GNOME/msitools.git /root/msitools \
	&& cd /root/msitools \
	&& autoreconf --install --force \
	&& ./configure --prefix=/usr/local \
	&& make -j$(nproc) \
	&& make install \
	&& rm -rf /root/msitools

# RUN freshclam

# temporary: install old version of file, as libmagic-5.33 crashes
RUN pacman -U --noconfirm https://archive.archlinux.org/packages/f/file/file-5.32-1-x86_64.pkg.tar.xz

RUN useradd -m -s /bin/bash runner
ENV GHCRTS=-N1 PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/cctools/bin LANG=en_US.UTF-8
COPY itchy-runner /usr/bin/itchy-runner
WORKDIR /home/runner
USER runner
