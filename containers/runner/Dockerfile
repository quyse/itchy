FROM base/archlinux

RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm git autoconf automake libtool clang llvm make unzip gcc-objc gnustep-base clamav

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

# build unar
RUN curl -Lo /root/unar.zip http://unarchiver.c3.cx/downloads/unar1.10.1_src.zip \
	&& mkdir /root/unar \
	&& cd /root/unar \
	&& unzip /root/unar.zip \
	&& rm /root/unar.zip \
	&& cd The\ Unarchiver/XADMaster \
	&& make -j$(nproc) -f Makefile.linux \
	&& mv unar /usr/bin/ \
	&& rm -r /root/unar

RUN freshclam

RUN useradd -m -s /bin/bash runner
ENV GHCRTS=-N1 PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/cctools/bin LANG=en_US.UTF-8
COPY itchy-runner /usr/bin/itchy-runner
WORKDIR /home/runner
USER runner
