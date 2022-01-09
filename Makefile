
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

# git clone of spacemacs develop branch
spacemacs :
	git clone -b develop https://github.com/emacs18/spacemacs $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos

# spacemacs worktree for 'develop' branch without any of my changes
sm-d : spacemacs
	cd spacemacs; \
	if `$(call GIT_HAS_BRANCH,sm-d)`; then \
	  git worktree add ../sm-d sm-d; \
	else \
	  git worktree add ../sm-d develop; \
	fi; \

# spacemacs worktree for 'develop' branch plus one change which is the
# (squashed) merge of sm-straight branch.
sm-s : spacemacs
	cd spacemacs; \
	git checkout develop; \
	if `$(call GIT_HAS_BRANCH,$@)`; then \
	  git worktree add ../$@ $@; \
	else \
	  git worktree add ../$@; \
	fi;
	mkdir -p $@/.local/straight
	cd $@/.local/straight; \
	  rm -f straight; \
	  ln -s ../../../straight/repos
	cd $@; \
	  git merge --squash sm-straight; \
	  git commit -a -m 'merged sm-straight branch'; \

# spacemacs worktree for 'develop' branch plus one change which is the
# (squashed) merge of sm-my branch.
sm-m :
	cd spacemacs; \
	git checkout develop; \
	if `$(call GIT_HAS_BRANCH,$@)`; then \
	  git worktree add ../$@ $@; \
	else \
	  git worktree add ../$@; \
	fi;
	cd $@; \
	  git merge --squash sm-my; \
	  git commit -a -m 'merged sm-my branch'; \

# spacemacs worktree for 'develop' branch plus these two changes:
# - squashed merge of sm-straight branch
# - squashed merge of sm-my branch
sm-ms :
	cd spacemacs; \
	git checkout develop; \
	if `$(call GIT_HAS_BRANCH,$@)`; then \
	  git worktree add ../$@ $@; \
	else \
	  git worktree add ../$@; \
	fi;
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

update-spacemacs : spacemacs
	cd spacemacs; git pull

update-m : update-spacemacs
	cd sm-my; git rebase develop
	cd sm-m; \
	  git reset HEAD~ \
	  && git reset --hard \
	  && git clean -fd -e .local \
	  && git rebase develop \
	  && git merge --squash sm-my \
	  && git commit -a -m 'merged sm-my branch' \
	  && git log --oneline --graph --format='%ai %h %an %s' -3 \

update-s : update-spacemacs
	cd sm-straight; git rebase develop
	cd sm-s; \
	  git reset HEAD~ \
	  && git reset --hard \
	  && git clean -fd -e .local \
	  && git rebase develop \
	  && git merge --squash sm-straight \
	  && git commit -a -m 'merged sm-straight branch' \
	  && git log --oneline --graph --format='%ai %h %an %s' -3 \

update-ms : update-m update-s
	cd sm-ms; \
	  git reset HEAD~2  \
	  && git reset --hard \
	  && git clean -fd -e .local \
	  && git rebase develop \
	  && git merge --squash sm-straight \
	  && git commit -a -m 'merged sm-straight branch' \
	  && git merge --squash sm-my \
	  && git commit -a -m 'merged sm-my branch' \
	  && git log --oneline --graph --format='%ai %h %an %s' -4 \

ve : vanilla-emacs

vanilla-emacs :
	git clone https://github.com/lccambiaghi/vanilla-emacs $@

push-all :
	cd spacemacs; git push
	cd sm-my; git push -f
	cd sm-straight git push -f
	cd sm-m; git push -f
	cd sm-s; git push -f
	cd sm-ms; git push -f

ls :
	cd spacemacs; git log --oneline --graph --format='%ai %h %an %s' -1; echo ' '
	cd sm-m; git log --oneline --graph --format='%ai %h %an %s' -2; echo ' '
	cd sm-s; git log --oneline --graph --format='%ai %h %an %s' -2; echo ' '
	cd sm-ms; git log --oneline --graph --format='%ai %h %an %s' -3; echo ' '
