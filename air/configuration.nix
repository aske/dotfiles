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
    kernelModules = [ "tun" "virtio" ];
    kernelPackages = pkgs.linuxPackages_latest;
#    kernelPackages = pkgs.linuxPackages_4_6;
    loader.timeout = 0;
#   plymouth.enable = true;
  };
	
  networking = {
    hostName = "nameless"; # Define your hostname.
    networkmanager.enable = true;
    firewall.enable = false;
    firewall.checkReversePath = false;
  };

  virtualisation.libvirtd.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-v18n.psf.gz";
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
    ];
  };

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;

    wine.release = "staging";

    bochs = {
      debugger = true;
      disasm = true;
      debuggerGui = true;
    };

    packageOverrides = pkgs: with pkgs; {
      gajim = gajim.override {
        extraPythonPackages = pkgs: [ pkgs.python-axolotl ];
      };
      mumble = mumble.override {
        speechdSupport = true;
        speechd = speechd.override {
          withEspeak = true;
        };
      };
      mpv = mpv.override {
        vaapiSupport = true;
      };
      deadbeef-with-plugins = deadbeef-with-plugins.override {
        plugins = [ deadbeef-mpris2-plugin ];
      };
    };
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
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
    
    p7zip
    zip
    unzip
    unrar
    tree
    rsync
    file
    pv
    dos2unix
   
    pastebinit
    
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
    
    firefox
    qutebrowser
    gajim
    mumble
    neomutt
    deluge
    tdesktop

    pavucontrol
    mpv

    xflux
    xflux-gui

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
      
    stylish-haskell
    hlint
    threadscope
    ])
]);

  # List services that you want to enable:
  services = {
    openssh.enable = true;
    tlp.enable = true;
    printing.enable = true;
    printing.gutenprint = true;
    upower.enable = true;

    chrony.enable = true;
    ntp.enable = false;

    postgresql = {
      enable = true;
      package = pkgs.postgresql95;
    };
    
    xserver = {
      enable = true;

      videoDrivers = [ "intel" ];
      deviceSection = ''
        Option "DRI" "2"
#        Option "TearFree" "true"
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
        '';
      };

      windowManager.default = "xmonad";
      windowManager.xmonad = {
        enable = true;
        extraPackages = self: with self; [ taffybar xmonad-contrib xmonad-extras ];
      };
      layout = "us,ru";
#      xkbOptions = "eurosign:e,grp:alt_space_toggle,grp_led:scroll,terminate:ctrl_alt_bksp";
      xkbOptions = "eurosign:e,ctrl:nocaps,grp:alt_space_toggle";
    };
  };

  programs = {
    zsh.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #  isNormalUser = true;
  #  uid = 1000;
  #};

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
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
    
    extraUsers = {
      root.passwordFile = "/root/.passwd";

      aske = {
        extraGroups = [ "wheel" "networkmanager" "libvirtd" ];
        uid = 1000; 
        isNormalUser = true;
        passwordFile = "/root/.aske.passwd";
      };
    };
  };

  environment.variables.EDITOR = "vim";

}
