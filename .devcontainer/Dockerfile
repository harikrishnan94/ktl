FROM gcc:latest

WORKDIR /root

ARG HOST_ARCH=x86_64

# Install cmake
ARG CMAKE_VERSION=3.25.2
RUN wget -c https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-linux-${HOST_ARCH}.sh && \
    chmod +x cmake-${CMAKE_VERSION}-linux-${HOST_ARCH}.sh && \
    ./cmake-${CMAKE_VERSION}-linux-${HOST_ARCH}.sh --skip-license --prefix=/usr/local/ && \
    rm cmake-${CMAKE_VERSION}-linux-${HOST_ARCH}.sh

# Install ninja
ARG NINJA_VERSION=1.11.1
RUN wget -c https://github.com/ninja-build/ninja/archive/refs/tags/v${NINJA_VERSION}.zip && \
    unzip v${NINJA_VERSION}.zip && \
    cd ninja-${NINJA_VERSION} && \
    cmake -B. && cmake --build . && \
    mv ninja /usr/local/bin/ && \
    cd .. && \
    rm -r v${NINJA_VERSION}.zip ninja-${NINJA_VERSION}

# Install AArch64-none-elf
# ARG TARGET_GCC_VERSION=12.2.rel1
# RUN wget -c https://developer.arm.com/-/media/Files/downloads/gnu/${TARGET_GCC_VERSION}/binrel/arm-gnu-toolchain-${TARGET_GCC_VERSION}-${HOST_ARCH}-aarch64-none-elf.tar.xz && \
#     tar xf arm-gnu-toolchain-${TARGET_GCC_VERSION}-${HOST_ARCH}-aarch64-none-elf.tar.xz && \
#     mv arm-gnu-toolchain-${TARGET_GCC_VERSION}-${HOST_ARCH}-aarch64-none-elf /usr/local/ && \
#     rm arm-gnu-toolchain-${TARGET_GCC_VERSION}-${HOST_ARCH}-aarch64-none-elf.tar.xz

ENV PATH "$PATH:/usr/local/arm-gnu-toolchain-${TARGET_GCC_VERSION}-${HOST_ARCH}-aarch64-none-elf/bin"

# Install gdb
ARG GDB_VERSION=12.1
RUN wget -c https://ftp.gnu.org/gnu/gdb/gdb-${GDB_VERSION}.tar.xz && \
    tar xf gdb-${GDB_VERSION}.tar.xz && \
    cd gdb-${GDB_VERSION} && \
    ./configure && make -j4 && make install -j && \
    cd .. && rm -r gdb-${GDB_VERSION}.tar.xz gdb-${GDB_VERSION}

# Install qemu-system-aarch64
# ARG QEMU_VERSION=7.2.0
# RUN wget -c https://download.qemu.org/qemu-${QEMU_VERSION}.tar.xz && \
#     tar xf qemu-${QEMU_VERSION}.tar.xz && \
#     cd qemu-${QEMU_VERSION} && \
#     ./configure --target-list=aarch64-softmmu && \
#     make install -j && \
#     cd .. && rm -r qemu-${QEMU_VERSION}.tar.xz qemu-${QEMU_VERSION}

# Install clang
ARG LLVM_VERSION=15.0.7
RUN wget -c https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/llvm-project-${LLVM_VERSION}.src.tar.xz && \
    tar xf llvm-project-${LLVM_VERSION}.src.tar.xz && \
    cd llvm-project-${LLVM_VERSION}.src && \
    cmake -Bbuild -Sllvm -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;lldb;lld" \
    -DLLVM_PARALLEL_LINK_JOBS=2 -DLLVM_TARGETS_TO_BUILD="host;AArch64" -G Ninja && \
    cmake --build build --target install && \
    cd .. && rm -r llvm-project-${LLVM_VERSION}.src.tar.xz llvm-project-${LLVM_VERSION}.src

# Create a 'dev' user
ARG DEV_USER_NAME=dev
RUN useradd ${DEV_USER_NAME} -d /home/${DEV_USER_NAME} && passwd -d ${DEV_USER_NAME} && \
    mkdir /home/${DEV_USER_NAME} && chown ${DEV_USER_NAME} /home/${DEV_USER_NAME}

# Locale Gen
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update && apt install -y locales
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    sed -i '/en_IN.UTF-8/s/^# //g' /etc/locale.gen && \
    sed -i '/ta_IN.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
