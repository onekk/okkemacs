;; ---------------------------------------------------------------------------
;; needs to be specified  before the (require)
;; place yous smtp server address
(setq smtpmail-default-smtp-server "")

(require 'smtpmail)

(setq mail-user-agent 'message-user-agent)

;; use port 465 and ssl for yahoo-mail
(setq send-mail-function    'smtpmail-send-it
  ;; user address for authentication
  smtpmail-smtp-user ""
  ;; mail smtp server
  smtpmail-smtp-server  ""
  ;; acces type see Emacs documentation of 'smtpmail'
  smtpmail-stream-type  'ssl
  smtpmail-smtp-service 465
  ;; setting a mail signature
  mail-signature ""
  smtpmail-mail-address ""
  ;; postponed message is put in the following draft file
  message-auto-save-directory "~/Mail/drafts"
  ;; optional, use it to set the message as mixed text and html content.
  message-default-mail-headers "Content-Type: text/html \n"
  ;; this should permit to choose a from field
  ;; list chould susbsitute <user-email-address> with <alternative address>
  ;; but I haven't managed to make it work 
  message-alternative-emails
  (regexp-opt '("<user-email-address>" "<alternative address>"))
  )
