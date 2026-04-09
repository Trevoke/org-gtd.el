FROM orchestrator-agent

# Install Emacs (28.1+ required) and git for the org-gtd.el project
# Handle both apt-based and apk-based base images
RUN if command -v apt-get >/dev/null 2>&1; then \
      apt-get update && \
      apt-get install -y --no-install-recommends \
        emacs-nox \
        git \
        curl \
        ca-certificates \
      && rm -rf /var/lib/apt/lists/*; \
    elif command -v apk >/dev/null 2>&1; then \
      apk add --no-cache \
        emacs-nox \
        git \
        curl \
        ca-certificates; \
    elif command -v dnf >/dev/null 2>&1; then \
      dnf install -y \
        emacs-nox \
        git \
        curl \
        ca-certificates \
      && dnf clean all; \
    elif command -v yum >/dev/null 2>&1; then \
      yum install -y \
        emacs-nox \
        git \
        curl \
        ca-certificates \
      && yum clean all; \
    else \
      echo "ERROR: No supported package manager found" && exit 1; \
    fi

# Verify emacs was actually installed
RUN emacs --version

# Install Eldev (Emacs build tool)
RUN curl -fsSL https://raw.github.com/doublep/eldev/master/webinstall/github-eldev | sh
ENV PATH="/root/.local/bin:${PATH}"
