#!/bin/bash
echo "Backuping original files in ./backup"
cp /etc/locale.conf ./backup
cp /etc/locale.gen ./backup
cp /etc/vconsole.conf ./backup
cp /etc/X11/xorg.conf.d/00-keyboard.conf ./backup

echo "Copying files.."
cp locale.conf /etc/
cp locale.gen /etc/
cp vconsole.conf /etc/
cp 00-keyboard.conf /etc/X11/xorg.conf.d/

echo "Generating locales"
locale-gen

echo "You should also probably update /etc/mkinitcpio.conf, see example in ../ for reference"
