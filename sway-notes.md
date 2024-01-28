# Arch Linux + Sway

Doing this will get you running with Sway. I'll add things to this as I remember them.

### Packages

Install these packages with pacman:

```
sudo pacman -S amixer \
			   blueman \
			   git \
			   keyd \
			   mako \
			   networkmanager \
			   python-i3ipc \
			   rofi \
			   sddm \
			   signal-desktop \
			   sway \
			   swaybg \
			   swayidle \
			   swaymsg \
			   waybar \
			   xwayland
```

Download the package manager yay like this:

```
sudo pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

Then download these packages with yay:

```
yay -S slack-desktop \
       swaylock-effects
```

### Download config files

Get your configuration files linked:

```
git clone https://github.com/snarisi/dotfiles.git ~/Personal/dotfiles
ln -s ~/Personal/dotfiles/sway/config ~/.config/sway/config
ln -s ~/Personal/dotfiles/waybar/config ~/.config/waybar/config
ln -s ~/Personal/dotfiles/waybar/style.css ~/.config/waybar/style.css
```

### Zsh

You're using the zim configurator thing, so get that first. Then run:

```
ln -s ~/Personal/dotfiles/.zshrc ~/.zshrc
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

And make sure you have the fonts Overpass Mono and Linux Libertine installed into ~/.fonts. You can get them from https://fonts.google.com/specimen/Overpass+Mono and https://www.dafont.com/linux-libertine.font.

### Wallpaper

You can include the wallpaper by linking it:

```
ln -s ~/Personal/dotfiles/spanish_civil_war.jpg ~/Pictures/wallpapers/spanish_civil_war.jpg
```
