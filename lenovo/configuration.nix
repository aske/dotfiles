# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix = {
    useSandbox = true;
    extraOptions = ''
      auto-optimise-store = true
    '';
  };

  boot = {
    supportedFilesystems = [ "ntfs" "exfat" ];
    kernelModules = [ "tun" "virtio" "vboxnetctl" "vboxdrv" ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader.timeout = 0;
  };

  networking = {
    hostName = "ins";
    networkmanager.enable = true;
    firewall.enable = false;
    firewall.checkReversePath = false;
  };

  virtualisation.libvirtd.enable = true;
  virtualisation.virtualbox.host.enable = true;

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-v22b.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ anthy mozc uniemoji ];
    };
  };

  fonts = {
    fonts = with pkgs; [
      corefonts
      terminus_font
      emojione
      cm_unicode
      xits-math
      dejavu_fonts
      noto-fonts
      noto-fonts-cjk
      source-code-pro
      fira-code
    ];
    fontconfig = {
      dpi = 96;
#      defaultFonts.monospace = [ "Source Code Pro" ];
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;

#    chromium = {
#      enablePepperPDF = true;
#    };

    packageOverrides = pkgs: with pkgs; {
      gajim = gajim.override {
        extraPythonPackages = pkgs: [ pkgs.python-axolotl ];
      };
      mpv = mpv.override {
        vaapiSupport = true;
      };
      deadbeef-with-plugins = deadbeef-with-pugins.override {
        plugins = [ deadbeef-mpris2-plugin ];
      };
      virtualbox = virtualbox.override {
        enableExtensionPack = true;
      };
    };
  };
  
  environment.systemPackages = lib.flatten (with pkgs; [
    mkpasswd
    wget
    vim
    lm_sensors
    htop
    iotop
    ftop
    powertop
    nethogs
    pciutils
    usbutils
    lsof
    psmisc

    btrfsProgs
    hdparm

    p7zip
    zip
    unzip
    unrar
    tree
    rsync
    file
    pv
    dos2unix
    ack

    gnupg
    
    nix-repl
    nix-prefetch-scripts
    git
    nox

    inetutils
    openvpn
    socat
    elinks
    mtr

    networkmanagerapplet
    networkmanager_openvpn

   screen
   parallel
   mkpasswd

   thunderbird
   
   firefox
   qutebrowser
   chromium
   gajim
   mumble
   tdesktop

   easyrsa
   
   qemu
   vagrant
   virtualbox 

   ffmpeg
   simplescreenrecorder
   youtube-dl

   patchelf

   pavucontrol
   mpv
   
   xkb_switch
   rxvt_unicode-with-plugins
   
   python2Full
   python3
   ruby
   jre
   clang
   gdb
   mercurial

   taffybar
   
   (with haskellPackages; [
     ((ghcWithPackages (self: with self; [ transformers mtl lens ])).override {
        withLLVM = true;
     })
     cabal-install
     cabal2nix
     hlint
    ])

  (emacsWithPackages (with emacsPackagesNg; [
    evil undo-tree key-chord linum-relative ace-jump-mode
    powerline-evil use-package projectile magit
    company company-quickhelp company-nixos-options company-jedi
    flycheck flycheck-pos-tip flycheck-haskell
    yasnippet
    nixos-options nix-sandbox
    haskell-mode
    python-mode
    ruby slim-mode
    nav
  ]))

  bundler_HEAD
  bundix
  
]);

  services = {
    openssh.enable = true;
    tlp.enable = true;
    printing.enable = true;
    printing.gutenprint = true;
    upower.enable = true;

    chrony.enable = true;
    ntp.enable = true;

#    nfs.server = {
#      enable = true;
#      exports = ''
#       
#      '';
#      mountdPort = 4002;
#      lockdPort = 4003;
#    };

    xserver = {
      enable = true;
      videoDrivers = [ "intel" ];
      deviceSection = ''
        Option "DRI" "3"
        Option "SwapBuffersWait" "false"
        Option "TripleBuffer" "true"
        Option "TearFree" "true"
      '';
      useGlamor = true;
      
      desktopManager = {
        default = "none";
      };

      displayManager = {
        sddm.enable = true;
        logToJournal = false;
        
        sessionCommands = ''
          xsetroot -solid black &
          taffybar &
          nm-applet &
          ibus-daemon -t refresh &
        '';
      };
   windowManager.default = "xmonad";
   windowManager.xmonad = {
     enable = true;
     extraPackages = self: with self; [ taffybar xmonad-contrib xmonad-extras ];
  };
  layout = "us,ru";
  xkbOptions = "eurosign:e,ctrl:nocaps,grp:alt_space_toogle";
  };
}; 

  programs = {
    zsh.enable = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

  powerManagement.scsiLinkPolicy = null;
  powerManagement.cpuFreqGovernor = null;

  hardware = {
    opengl = {
      driSupport32Bit = true;
      s3tcSupport = true;
    };
    pulseaudio = {
      package = pkgs.pulseaudioFull;
      support32Bit = true;
      enable = true;
    };
    cpu.intel.updateMicrocode = true;
    amdHybridGraphics.disable = true;
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
    extraUsers = {
      root.passwordFile = "/root/.passwd";
      
      aske = {
        extraGroups = [ "wheel" "networkmanager" "libvirtd" "vboxusers" ];
        uid = 1000;
        isNormalUser = true;
        passwordFile = "/root/.aske.passwd";
      };
    };
  };
  environment.variables.EDITOR = "vim";
}
