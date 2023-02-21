FROM ubuntu:latest

WORKDIR /root

# Locale Gen
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update && apt install -y g++-12 gdb git python3-pip clang-15 clangd-15 clang-tidy-15 locales wget sudo

# Install cmake and ninja
RUN pip3 install cmake ninja

# Create a 'dev' user
ARG DEV_USER_NAME=dev
RUN useradd ${DEV_USER_NAME} -d /home/${DEV_USER_NAME} && passwd -d ${DEV_USER_NAME} && \
    mkdir /home/${DEV_USER_NAME} && chown ${DEV_USER_NAME} /home/${DEV_USER_NAME} && \
    usermod -aG sudo ${DEV_USER_NAME} && \
    echo "$DEV_USER_NAME ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee /etc/sudoers.d/${DEV_USER_NAME}

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    sed -i '/en_IN.UTF-8/s/^# //g' /etc/locale.gen && \
    sed -i '/ta_IN.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
