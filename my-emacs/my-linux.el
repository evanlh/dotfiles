(provide 'my-linux)
(if (eq system-type 'gnu/linux)
    (progn
      (menu-bar-mode -1)
      (set-frame-font "-adobe-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")))
