;;; -*- lexical-binding: t -*-

(install-package 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(benchmark-init/activate)


(provide 'init-benchmark)
;;; init-benchmark.el ends here
