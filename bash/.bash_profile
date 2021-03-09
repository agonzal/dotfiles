# .bash_profile


# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
PATH=$PATH:/opt/metasploit-framework/bin:/opt/gradle/bin:/home/linuxbrew/.linuxbrew/bin:/home/electr0n/.cargo/bin
eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
eval $(/bin/brew shellenv)
