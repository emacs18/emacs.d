
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

sm-d :
	git clone -b develop https://github.com/syl20bnr/spacemacs $@

# Fork spacemacs to prepare for straight.el on branch named 'straight'.
sm-s :
	git clone -b straight https://github.com/emacs18/spacemacs $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos

sm-m :
	git clone -b develop --reference ~/.emacs.d/sm-ms git@github.com:emacs18/spacemacs $@

sm-ms :
	git clone -b develop https://github.com/emacs18/spacemacs $@
	mkdir -p $@/.local/straight
	cd $@/.local/straight; ln -s ../../../straight/repos

ve : vanilla-emacs

vanilla-emacs :
	git clone https://github.com/lccambiaghi/vanilla-emacs $@
