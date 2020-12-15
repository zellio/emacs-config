;;; config/99_theme.el --- load emacs theme

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package zenburn-theme
  :init (setq
         custom-safe-themes
         '("04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5"
           "0c9f63c9d90d0d135935392873cd016cc1767638de92841a5b277481f1ec1f4a"
           "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df"
           "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077"
           "710ea302b6981bc7450808a182fb2e08a7363ffeddbe2d49843603442d2d34de"
           "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77"
           "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f"
           "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9"
           "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba"
           "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70"
           "f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef"))

  :config (progn
            (add-to-list 'custom-theme-load-path package-user-dir)
            (load-theme 'zenburn)))

;;; config/99_theme.el ends here
