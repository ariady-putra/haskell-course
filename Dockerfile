FROM ghcr.io/jamesdbrock/ihaskell-notebook:master



# set up a user whose uid is 1000. It is bad practice to run processes in containers as root, and on binder we do not allow root container processes.
ARG NB_USER=rober
ARG NB_UID=1000
ENV USER ${NB_USER}
ENV NB_UID ${NB_UID}
ENV HOME /home/${NB_USER}

USER root



# ---- As advised here:
#    https://github.com/binder-project/binder/issues/50
RUN pip install jupyter_client

# ---- Install RISE extension
RUN cd ~/src/git/ && \
    git clone https://github.com/damianavila/RISE && \
    cd ~/src/git/RISE && \
    python setup.py install




#RUN adduser --disabled-password \
#    --gecos "Default user" \
#    --uid ${NB_UID} \
#    ${NB_USER}
    
# Make sure the contents of our repo are in ${HOME}
COPY . ${HOME}

RUN chown -R ${NB_UID} ${HOME}
USER ${NB_USER}
