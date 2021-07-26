
GIT_HAS_BRANCH = git branch | grep -q -e "\b$(1)\b"

all : straight chemacs2 doom purcell-s scimax-s sm-s

straight :
	mkdir -p straight/repos

chemacs2 :
	git clone https://github.com/plexus/chemacs2 $@
	test -h init.el || ln -s $@/init.el

doom :
	git clone -b develop https://github.com/hlissner/doom-emacs $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos
	export DOOMDIR=$(PWD)/doom/.doom.d; mkdir -p $$DOOMDIR; cd doom; ./bin/doom install

purcell :
	git clone https://github.com/purcell/emacs.d $@

# Fork purcell to prepare for straight.el on branch named 'straight'.
purcell-s :
	git clone -b straight https://github.com/emacs18/purcell.git $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos

scimax :
	git clone https://github.com/jkitchin/scimax $@

# Fork scimax to prepare for straight.el on branch named 'straight'.
scimax-s :
	git clone -b straight https://github.com/emacs18/scimax.git $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos

spacemacs :
	git clone -b develop https://github.com/emacs18/spacemacs $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos

sm-d : spacemacs
	cd spacemacs; \
	if `$(call GIT_HAS_BRANCH,sm-d)`; then \
	  git worktree add ../sm-d sm-d; \
	else \
	  git worktree add ../sm-d develop; \
	fi; \

sm-s : spacemacs
	cd spacemacs; \
	git checkout develop; \
	if `$(call GIT_HAS_BRANCH,$@)`; then \
	  git worktree add ../$@ $@; \
	else \
	  git worktree add ../$@; \
	fi; \
	mkdir -p $@/.local/straight
	cd $@/.local/straight; \
	  rm -f straight; \
	  ln -s ../../../straight/repos
	cd $@; \
	  git merge --squash sm-straight; \
	  git commit -a -m 'merged sm-straight branch'; \

sm-m :
	cd spacemacs; \
	git checkout develop; \
	if `$(call GIT_HAS_BRANCH,$@)`; then \
	  git worktree add ../$@ $@; \
	else \
	  git worktree add ../$@; \
	fi; \
	cd $@; \
	  git merge --squash sm-my; \
	  git commit -a -m 'merged sm-my branch'; \

sm-ms :
	cd spacemacs; \
	git checkout develop; \
	if `$(call GIT_HAS_BRANCH,$@)`; then \
	  git worktree add ../$@ $@; \
	else \
	  git worktree add ../$@; \
	fi; \
	mkdir -p $@/.local/straight
	cd $@/.local/straight; \
	  rm -f straight; \
	  ln -s ../../../straight/repos
	cd $@; \
	  git merge --squash sm-straight; \
	  git commit -a -m 'merged sm-straight branch'; \
	cd $@; \
	  git merge --squash sm-my; \
	  git commit -a -m 'merged sm-my branch'; \

ve : vanilla-emacs

vanilla-emacs :
	git clone https://github.com/lccambiaghi/vanilla-emacs $@
