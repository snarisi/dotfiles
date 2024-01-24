# Sway notes

This is a temporary file just to write down the things that I might forget.

### Packages

Install these packages with pacman:

```
sudo pacman -S xcape \
			   waybar \
			   mako \
			   amixer \
			   sway \
			   swayidle \
			   swaylock \
			   swaymsg \
			   networkmanager \
			   blueman \
			   xwayland \
			   sddm \
			   rofi \
			   signal-desktop \
			   python-i3ipc
```

Download the package manager yay like this:

```
pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

Then download these packages with yay:

```
yay -S slack-desktop
```

### Download config files

Get your configuration files linked:

```
git clone https://github.com/snarisi/dotfiles.git ~/Personal/dotfiles
ln -s ~/Personal/dotfiles/sway/config ~/.config/sway/config
ln -s ~/Personal/dotfiles/waybar/config ~/.config/waybar/config
ln -s ~/Personal/dotfiles/waybar/style.css ~/.config/waybar/style.css
```

### Emacs

Download the git repo somewhere, then go to the folder and run:

```
./autogen.sh
./configure
make
sudo make install
```

Then link your files to the right place:

```
git clone https://github.com/snarisi/samacs.git
cd samacs
ln -s ~/Personal/samacs/* ~/.emacs.d/
```
