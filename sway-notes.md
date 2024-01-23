# Sway notes

This is a temporary file just to write down the things that I might forget.

### Packages

Install these packages with pacman:

```
sudo pacman -S flatpak \
	           xcape \
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
			   rofi
```

And you're pretty sure you got this with flatpak:

```
flatpak install signal
```

### Download config files

Get your configuration files linked:

```
git clone https://github.com/snarisi/dotfiles.git ~/Personal/dotfiles
ln -s ~/Personal/dotfiles/sway/config ~/.config/sway/config
ln -s ~/Personal/dotfiles/waybar/config ~/.config/waybar/config
ln -s ~/Personal/dotfiles/waybar/style.css ~/.config/waybar/style.css
```
