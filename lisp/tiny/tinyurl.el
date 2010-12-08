;;; tinyurl.el --- Mark and jump to any URL on current line.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; Look at the code with folding.el.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;      (require 'tinyurl)
;;
;;      ;; To activate the mode globally add code below
;;      ;; Alternatively call global mode M-x tinyurl-mode or
;;      ;; buffer based mode M-x tinyurl-mode-1
;;
;;      (turn-on-tinyurl-mode)
;;
;;      ;; If you do not have permenent internet connection, add this
;;      ;; It will follow Gnus plugged mode state.
;;
;;      (setq tinyurl--plugged-function 'tinyurl-plugged-p)
;;
;;      ;; OPTIONAL: Attach to VM, RMAIL, Gnus etc.
;;      ;; (add-hook 'tinyurl--load-hook 'tinyurl-install-to-packages)
;;
;; You can also use the preferred way: autoload. Only when you call the
;; `M-x' `tinyurl-mode', this package loads. The following setup is
;; faster than above, but it doesn't install this package automatically to
;; VM, RMAIL, MH, gnus as the `require' method does. Call
;; `M-x' `tinyurl-install-to-packages' for those.
;;
;;      (autoload 'tinyurl-mode               "tinyurl" "" t)
;;      (autoload 'tinyurl-mode-1             "tinyurl" "" t)
;;      (autoload 'turn-on-tinyurl-mode-1     "tinyurl" "" t)
;;      (autoload 'turn-off-tinyurl-mode-1    "tinyurl" "" t)
;;
;;      ;; OPTIONAL: Attach to VM, RMAIL, Gnus etc.
;;      ;; (add-hook 'tinyurl--load-hook 'tinyurl-install-to-packages)
;;
;;      ;;  Keybinding suggestions
;;
;;      (global-set-key "\C-cmuu"  'tinyurl-mode)
;;      (global-set-key "\C-cmu1"  'tinyurl-mode-1)
;;      (global-set-key "\C-cmup"  'tinyurl-plugged-mode-toggle)
;;
;;      ;;  Select backend for EMAIL urls. See variable's documentation.
;;      (setq mail-user-agent 'message-user-agent)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, oct 1997
;;
;;      One day a collegue had a problem with his VM and he
;;      explained to me that he wanted the `mouse-2' to run netscape
;;      browser instead of the default Emacs `w3' browser. While he was
;;      waving his cursor over the http link, I suddendly realized: that
;;      this would be useful in RMAIL buffers too. (I later moved straight to
;;      GNUS). It seemed that every package had its own url handling: VM, TM,
;;      GNUS, MH.
;;
;;      But really, how about the rest of the buffers and modes? There was
;;      no general ULR dispatcher minor mode that would work with any buffer
;;      and with any mode.
;;
;;      Now there is; it is possible browse any buffer or document and
;;      jump to URLs on the line. Works for programing modes too.
;;      Position the cursor somewhere on the line, wait 2 seconds and
;;      the URLs in the current line are buttonized.
;;
;;  Overview of features
;;
;;      o   Requirements: XEmacs must contain package `overlay.el'.
;;          Emacs needs nothing special.
;;
;;      o   General URL handler: not just the regular http, ftp, but
;;          also for programming languages like Perl/Lisp/C++ and
;;          man page cut(1) references and jumping to Debian bug
;;          reports (+ WNPP) and more.
;;
;;      o   When the global minor mode is on, wait few seconds and the
;;          current line will be scanned for urls. Because not all
;;          terminals show color, there is additional "!" character added to
;;          the front of URL for calling you to *push* it.
;;      o   Once the minor mode is turned on, it occupies every buffer,
;;          but there is also function to turn the mode on or off per buffer
;;          basis, see `tinyurl-mode-1'. When new file is loaded,
;;          `tinyurl-mode' is activated for the buffer too.
;;      o   Defines binding `mouse-2' and `M-RET' to call the url at
;;          point. These bindings are electric: If there is no button to push,
;;          the original binding is called according to underlying mode.
;;      o   You can change the url handler sets on the fly: e.g.
;;          call lynx for a while, then switch to default browser
;;          or use your custom browser. See `M-x' `tinyurl-set-handler'
;;      o   Centralised url handling. If you call `tinyurl-install-to-packages'
;;          then GNUS, TM, VM etc. now call TinyUrl and you only need to
;;          configure things in one place.
;;
;;  Turning the URL recognizer on
;;
;;      Load package with `require' or via autoload (see installation
;;      instruction at the top of file) and call `M-x' `tinyurl-mode' to
;;      toggle the global minor mode on and off. The modeline displays `Ux'
;;      when the mode is active. A character like (x) is a short name
;;      for browser that will activate, e.g. "n" for "netscape" browser,
;;      (l) for lynx and (w) w3.
;;
;;      If you want to turn the mode on or off for current buffer only, use
;;      `M-x' `tinyurl-mode-1'.
;;
;;      The minor mode is turned on for all newly created (C-x C-f) or
;;      visited files, but if you make a new buffer with `M-x'
;;      `switch-to-buffer', the URL mode is not turned on in those buffers.
;;
;;  Caching URLs for later use (offline reading)
;;
;;      The offline reading is possible with Gnus, where status can be
;;      toggled between "plugged" and "unplugged". If variable
;;      `tinyurl--plugged-function' is set to default unpluggged condition
;;      detector function `tinyurl-plugged-p', it returns nil if Gnus is in
;;      unplugged state.
;;
;;      The current implementation relies on gnus (`M-x' `gnus') to detect
;;      the off-line, on-line status of the network connection. This
;;      means that all "buttons" are cached to separate buffer unless you
;;      tell that you're connected via `M-x' `gnus-agent-toggle-plugged'.
;;
;;      You can place your own unplugged state detector to variable
;;      `tinyurl--plugged-function'. Cache buffer used is
;;      `tinyurl--url-cache-buffer', which is *URL-cache* by default.
;;
;;      You can force TinyUrl to change plug status by calling `M-x'
;;      `tinyurl-plugged-mode-toggle'. This internal flag overrides anything
;;      else in the system. The indicator "!" in the modeline tells if
;;      TinyUrl thinks it is in plugged state. You may need to call this
;;      function if you don't use Gnus as a primary MUA.
;;
;;      Shortly:
;;
;;      o   If you use Gnus, toggle Agent with J j to plugged/unplugged
;;          and TinyUrl will follow Gnus's state.
;;      o   If you don't use gnus, or do not have it loaded, call
;;          function `tinyurl-plugged-mode-toggle' to tell the state of the
;;          net connection.
;;
;;  Editing the url and selecting access method manually
;;
;;      You can pass a prefix argument like `C-u' before you press
;;      `mouse-2' or `M-RET' and edit two parameters: a) The URL location
;;      itself and b) the access method. Say e.g. that your default command
;;      table is netscape and you see url
;;
;;          file:/users/foo/file.txt
;;
;;      The `file:/' would be normally considered external and accessed via
;;      `url' method, which in this case is netscape. But you would like
;;      to use Emacs `find-file' instead. Send `C-u' and leave the url as
;;      is and change access method to:
;;
;;          file
;;
;;      That's it. Remember however that you have full control and
;;      if you choose nonsense access method, which has nothing to do with
;;      the url, then you also carry the results, whatever they may be.
;;
;;  Ignoring URL in the buffer
;;
;;      You can use hook `tinyurl--dispatch-hook' to check URL. If any of
;;      the functions return t, then the original binding in the mode is
;;      called and the TinyUrl is not used. E.g. In Dired buffer you want to
;;      ignore all URLs. There is default function
;;      `tinyurl-dispatch-ignore-p' that does just this.
;;
;;  Centralised URL handling
;;
;;      If you called `M-x' `tinyurl-install-to-packages' or had installation:
;;
;;          (add-hook 'tinyurl--load-hook  'tinyurl-install-to-packages)
;;
;;      then GNUS, VM, TM, and other packages redirect urls to TinyUrl.
;;      This way you don't have to setup each package to your taste.
;;      Plus you got the benefit that you can change url handler set
;;      on the fly with `tinyurl-set-handler'.
;;
;;  Ignoring some buffers for mode turn on and offs
;;
;;      If you want to exclude some buffers from the mode turn on or offs,
;;      say *VM* which does its own highlighting, then define your
;;      custom function like this
;;
;;          (setq tinyurl--exclude-function 'my-tinyurl-exclude)
;;
;;          (defun my-tinyurl-exclude (buffer)
;;             "Exclude some buffers that use their own highlighting."
;;             (string-match "VM\\|Article" (buffer-name buffer)))
;;
;;      This only concern the golobal `tinyurl-mode' function. You can
;;      still use `tinyurl-mode-1' anywhere to toggle the mode setting.
;;      You use this variable when you don't want `tinyurl-mode' to
;;      appear in buffer at all.
;;
;;  Validating url
;;
;;      The `tinyurl-mark-line' function doesn't check the validity of a
;;      matched regexp that was marked as pushable url. It's a dummy
;;      function that can only attach "buttons" and does nothing about
;;      their contents. But when you actually push the url, the url is run
;;      through functions in `tinyurl--validate-hook'. When any of the
;;      function returns t, it is a *go* sign. The default handler
;;      `tinyurl-validate-url-default' rejects any url that matches
;;      "foo|bar|quux".
;;
;;      See also `tinyurl--reject-url-regexp' for more simpler use.
;;
;;  Choosing what agent handles which URL
;;
;;      There is predefined `tinyurl--command-table' which is consulted where
;;      URL request should be delegated. By default http:// or ftp:/ or file:/
;;      requests are handed by `browse-url-netscape' and remote tar or gz
;;      fileas are loaded with ange-ftp.
;;
;;      You can completely customize the URL delegation by writing your
;;      own url handler set and placing it to `tinyurl--url-handler-function'.
;;      Copy the default setup and make your own modifications.
;;
;;  Changing the url handler list
;;
;;      When you click the url to run the viewer, the current url handler
;;      list determines what method is used. E.g. If you normally want
;;      netscape to handle your URL, then the current set is labelled
;;      "netscape". But in some situations, where you want to e.g. view text
;;      files or your resources in PC EXceed are low, or you want fast browser;
;;      then there is also "lynx" set. You change the browser set with command
;;
;;          tinyurl-set-handler   Meta mouse-2
;;
;;      The modeline will show the first string from your active set; `Un'
;;      for Netscape, `Ul' for lynx set and `Uw' for w3 based set. You can
;;      add as many handler sets as you want by adding them to
;;      `tinyurl--command-table'
;;
;;  Exclamation character marks pushable URL
;;
;;      NOTE: THE VISIBLE CHACTER APPLIES ONLY TO TERMINALS THAT DO NOT
;;      SUPPORT COLORS TO MARK PUSHABLE URLS. (Usually an Emacs started
;;      with -nw, or running inside a terminal.)
;;
;;      When you see character "!" (netscape) or "?" (W3 browser) to appear
;;      in the front of the URLs, then you know that items are pushable.
;;      You can call the URL by clicking it with `mouse-2' or tapping
;;      `M-RET'. In the following line, two url's have been detected. The
;;      first one sends normal http request and the second one would create
;;      mail buffer for the address.
;;
;;          Some previous line here
;;          !http://foo.com/dir/file.txt  !<foo@bar.com>
;;          Another line below
;;
;;      Elswhere your `mouse-2' and `M-RET' behave as usual. If you would
;;      like to paste(the mouse-2) somewhere in the "previous" or "another"
;;      line, that would work as you expected. But you can't paste inside
;;      the URL, because the URL is currently activated. If you need to do
;;      something like that, then you can use either of these strategies:
;;
;;      o   Use `C-y' to yamk the text inside marked url.
;;      o   move cursor out of the URL line; wait few seconds for
;;          "!" to disappear (the line is cleared). Go back and paste before
;;          you see "!" to appear back again.
;;      o   Turn off the mode off with `M-x' `tinyurl-mode-1' for a while if
;;          you don't need the URL features right now.
;;
;;        _Note_: The character "!" that you see, is not a real editable
;;        character, but part of the overlay. While your text may appear to
;;        be modified. That is not what happened. See Emacs info pages for
;;        more about overlays.
;;
;;      You can use variable `tinyurl--display-glyph' to control if the
;;      glyph is shown or not.
;;
;;
;;  Accepted email URL
;;
;;      The default accepted format is <foo@site.com> and if you see
;;      foo@site.com, that will not be recognized. Your can get this
;;      accepted by changing `tinyurl--email-regexp'. You could use \\< and
;;      \\> (word border marker) regexps instead of default characters < >.
;;
;;  Support for programming language URLs
;;
;;      I'll gladly support any other languages. If you know the language
;;      you're using, drop me a mail and help me to undertand how I would
;;      add support to it. Especially I'd like to hear specs from Java
;;      programmers.
;;
;;     C/C++
;;
;;      The default agent to find C/C++ .h files is find-file.el's
;;      `ff-find-other-file'. This will handle your #include urls.
;;
;;     Perl
;;
;;      There is support for these perl statements:
;;
;;          use package;
;;          require package;
;;
;;      Functions that recognize those are under `tinyurl-find-url-perl*'.
;;      The default find path for perl is `@INC'. Perl related urls are
;;      delegated to separate tinyperl.el package. In addition perl compile
;;      error lines are recognized:
;;
;;          ERROR at FILE line NBR.
;;
;;      Perl pod page references are recognized in the format
;;
;;          perlfunc manpage
;;          See [perltoc]
;;
;;     Emacs lisp
;;
;;      The url handler function is `tinyurl-find-url-lisp' and Emacs
;;      `load-path' is searched. The usual urls "load-file", "load-library"
;;      "autoload" "load" are recognized. If you need to jump to function
;;      or variable definitions, you want to use a TinyLisp package, which
;;      offers minor mode solely for Emacs lisp programming purposes:
;;      Profiling, debugging, snooping hooks, you emacs packages, browsing
;;      code etc.
;;
;;     Other languages
;;
;;      Please let me know if you know package or you have code that can
;;      find other languages' URLs.
;;
;;     Debian support
;;
;;      Debian <http://www.debian.org> uses mail based bug tracking system
;;      where each assigned task is uniquely identified. The task can
;;      be a regular bug report send via command reportbug(1) or it can
;;      be a control message where developers can hand over maintenance
;;      of packages or in turn take over maintenance of orphaned packages.
;;      Visit page <http://www.debian.org/devel/wnpp> to see what the
;;      messages look like. The messages are best monitored and read
;;      through Gnus NNTP backend using newsgroup
;;      *nntp+news.gmane.org:gmane.linux.debian.devel.wnpp*. URLs like
;;      this are buttonized (requires package tinydebian.el):
;;
;;          Bug#NNNNNN
;;
;;     Memory list
;;
;;      o   Remember to define `ff-search-directories' for *find-file.el*
;;          so that your C/C++ #include <url> will be found correctly.
;;
;;  Filename filter e.g. running catdoc for MS Word files
;;
;;      There is table `tinyurl--file-filter-table' which can be used to
;;      handle found url. Eg if you want to treat all files ending
;;      to extension .doc as MS word files and feed them through
;;      `catdoc' http://www.ice.ru/~vitus/works/ which spits 7bit
;;      out, you can associate shell action to handle url. Respectively
;;      if you want to use `xv' for viewing your images, you can associate
;;      that to the url. The default table handles these cases if you
;;      have xv and catdoc present. See variable description for more
;;      information. (You can also use your custom lisp url handler there)
;;
;;        If you want to load the raw file into emacs, just supply
;;        prefix argument when you push url and you will be given choice
;;        to by-pass the set filters (if there is any) for the url.
;;
;;  Code note: adding buttons to the current line
;;
;;      The idle timer process is used to mark current line's urls with
;;      overlays. Please wait few seconds on a line and the ulrs that
;;      can be *pushed* are marked. If there is no idle timer available,
;;      then a `post-command-hook' is used.
;;
;;     [Next applies only to Emacs with no `run-with-idle-timer' function]
;;
;;      Using `post-command-hook' is not an ideal solution, but at least
;;      this package works with older Emacs versions. The threshold how
;;      quicly the line is scanned for url buttons is determined by
;;      variable `tinyurl--post-command-hook-threshold'. The deafult value
;;      7 should give you enough time to use `mouse-2' (paste) before the
;;      line is buttonized. Remember that *vawing* you mouse creates
;;      events, so you can force buttonizing the line quite quickly.
;;
;;  Code note: overlay properties
;;
;;      The overlays have nice feature where you can add string to be
;;      displayed to the side of an overlay. See the overlay properties in
;;      the Emacs info pages for more. The overlay `priority' in this
;;      package is by default set to highest possible, so that the URL
;;      highighting is guarranteed to be dislayed. If you use some other
;;      package that also uses overlays, then decrease that package's
;;      overlay priorities. (If the package doesn't allow you to adjust the
;;      priorities, contact the package maintainer. To my opinion the
;;      priority value should be defined for all overlays).
;;
;;      The only part that you should touch in the property list of the
;;      overlays, is the displayed string. You can choose anything you
;;      want, but prefer one character. By default the "!" is shown in
;;      both Windowed and non-windowed version.
;;
;;      The overlays have property `owner' which tells to whom
;;      particular overlays belong. In this case the owner is this package,
;;      `tinyurl'. It is a good practise for all overlays to identify
;;      themselves via this 'owner property.
;;
;;  Code Note: overlay management
;;
;;      Let's consider what `font-lock' does for buffer for a moment: it
;;      marks whole buffer with faces (colors). While design this package,
;;      the goal was not to add buffer with full of clickable overlays,
;;      while that could have been done easily. The reason is efficiency
;;      and avoiding "highlight" bloat.
;;
;;      Instead old overlays are removed and new ones are created only for
;;      current line, typically the count is between 1 .. 4. When you move
;;      to another place, these old overlays are destroyed and new ones
;;      created. The current line may now may have only 1 URL, so only one
;;      overlay was needed this time.
;;
;;      For that reason you must wait for idle timer process to do its
;;      work on current line, before you can see those clickable URL
;;      buttons.
;;
;;      Using only small number of overlays keeps the code clean and user
;;      friendly. It's also faster than buttonizing whole 500K faq
;;      document in one pass.
;;
;;  Code Note: Adding support for new URL type
;;
;;      If you see new url that you would like to have supported and you
;;      know lisp, then the changes needed are:
;;
;;      o   `tinyurl-mark-line', Add regexp to match the URL. Think carefully
;;          where to put the regexp and make is as restrictive as you can.
;;          Remember that first OR match is picked.
;;      o   `tinyurl-type', Add new type for URL
;;      o   `tinyurl-command-table-default-1' Add default handler
;;      o   Write the URL handler.
;;      o   Run `tinyurl-command-table-defaults-set' to make the new handler
;;          seen in the default agent function list
;;
;;      To make changes do this:
;;
;;      o   copy original version to `tinyurl.el.orig'
;;      o   Make changes
;;      o   Produce diff `diff -b -w -u  tinyurl.el.orig tinyurl.el'
;;
;;      Then send diff to the maintainer. Use unified diff format (-u) if
;;      possible. Second chance is to use context diff (-c). Other diff
;;      formats are not accepted.
;;
;;  Sending a bug report
;;
;;      If you have a line where url is highlighted, but it doesn't cover
;;      right characters, then do this:
;;
;;      o   `M-x' `tinyurl-submit-bug-report'
;;      o   Copy the _WHOLE_ line to the mail buffer.
;;      o   Turn on debug with `M-x' `tinyurl-debug-toggle'
;;      o   Be sure Url gets highlighted. End debug with
;;          `M-x' `tinyurl-debug-toggle' and copy the content of
;;          *tinyurl-debug* to the mail
;;      o   Attach desctiption of the bug and send mail.
;;
;;      Btw, in win32 the file url on `C:' disk is written like
;;
;;          file://localhost/C|/foo/bar/baz.html#here
;;
;;      And according to RFC, if you leave out the <host>, the localhost is
;;      automatically assumed.
;;
;;          file:///C|/foo/bar/baz.html#here
;;
;;  Known Bugs
;;
;;      The URL is highlighted by setting `mouse-face' to property
;;      `highligh'. But I have seen that Emacs 19.34 in HP Unix with X
;;      window sometimes won't show the highlight when cursor is moved
;;      over the URL. Go figure why. I have heard similar reports from
;;      XEmacs 20.4.
;;
;;      If you know what is causing this effect, let me know.
;;
;;  Todo
;;
;;      Add support for Java-Find.el

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(eval-when-compile
  (require 'advice))

(eval-and-compile
  (defvar gnus-plugged)
  (defvar browse-url-browser-function)
  (defvar gnus-button-url)
  (defvar vm-url-browser)
  (defvar browse-url-browser-function)
  (autoload 'man                                "man"       "" t)
  (autoload 'ffap                               "ffap"      "" t)
  (autoload 'ff-find-other-file                 "find-file" "" t)
  (autoload 'tinydebian-bug-browse-url-by-bug   "tinydebian"  "" t)
  (autoload 'tinyperl-pod-by-manpage            "tinyperl"  "" t)
  (autoload 'tinyperl-pod-by-module             "tinyperl"  "" t)
  (autoload 'tinyperl-pod-manpage-to-file       "tinyperl"  "" t)
  (autoload 'tinyperl-locate-library            "tinyperl"  "" t)
  (autoload 'tinyperl-library-find-file         "tinyperl"  "" t)
  (autoload 'turn-on-tinyperl-pod-view-mode     "tinyperl"  "" t)
  (ti::overlay-require-macro
    (message "\
** tinyurl.el: Error, this Emacs does not have overlay functions.")))

(ti::package-defgroup-tiny TinyUrl tinyurl-- extensions
  "Global URL highlighting and dispatcher minor mode.")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinyurl--load-hook '(tinyurl-install-to-packages)
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyUrl)

(defcustom tinyurl--dispatch-hook  '(tinyurl-dispatch-ignore-p)
  "When calling urls, check if it is allowed.
this hook's purpose is to check current buffer, current line or anything
else to determine if pushing URL is ste wanted action. Eg in dired
buffer the pushing acting should not be respected but passed back
to Dired.

Default function in this hook is `tinyurl-dispatch-ignore-p'.

Function call arguments:

  url                   Matched url text
  '(buffer . point)     Pointer to location of url in Emacs

Function should return:

  non-nil   To ignore urls and pass control back to underlying mode.
  nil       Accept url and proceed."
  :type  'hook
  :group 'TinyUrl)

(defcustom tinyurl--validate-hook '(tinyurl-validate-url-default)
  "Validate called url. If some of these functions return t, url is accepted.

Function call arguments:

  string:    URL

Function should return:

  t         Accept and continue with url
  string    Display message STRING and ignore url
  nil       Display default message 'url ignored' and ignore url"
  :type  'hook
  :group 'TinyUrl)

;;; .......................................................... &public ...

(defcustom tinyurl--auto-activate-function
  'turn-on-tinyurl-mode-automatically
  "*Function to check if there are URLs in current buffer.
This function will automatically turn on `tinyurl-mode-1' for the
current buffer it it returns t."
  :type  'function
  :group 'TinyUrl)

(defcustom tinyurl--plugged-function  'tinyurl-plugged-always-p
  "Function to determine disconnected state.
Function takes no arguments and should return t if Emacs is disconnected
and unable to serve external URL requests.

Possible values:
  'tinyurl-plugged-always-p
  'tinyurl-plugged-p

See also: `tinyurl--url-cache-buffer'"
  :type  'function
  :group 'TinyUrl)

(defcustom tinyurl--exclude-function 'tinyurl-default-exclude
  "*Function to prohibit (de)activatiting `turl-mode' for a buffer.
This function is called when TinyUrl mode is booted up or shut down.

Function call argument:

  buffer-pointer

Function should return:

  t         if buffer is ignored"
  :type  'function
  :group 'TinyUrl)

(defcustom tinyurl--display-glyph (not (ti::colors-supported-p))
  "*If non-nil, Display the Overlay glyph: !, ? or *.

The shown character depends on the active command table.
If you have non-windowed Emacs which cannot
display faces on tty, then make sure this variable is t or you won't
notice the buttonized urls.

In Windowed Emacs the glyph may be redundant, because the face
property already highlights the URLs. Try if you like setting nil better in
non windowed Emacs."
  :type  'boolean
  :group 'TinyUrl)

(defcustom tinyurl--file-filter-table
  (let ((doc    (executable-find "catdoc"))
	(gimp   (executable-find "gimp"))
	(xv     (executable-find "xv"))
	(nroff  (executable-find "nroff"))
	(col    (executable-find "col"))
	(winzip (executable-find "winzip")))
    (list
     '("\\.pod$" . tinyurl-filter-pod)
     (if doc
         (cons "\\.doc$"  (concat doc " %s"))) ;View MS WORD files
     (cons "\\.\\(jpg\\|jpeg\\|gif\\)$"
           (cond
            (gimp
             (concat gimp " %s"))
            (xv
             (concat xv " %s"))
            (t
             'ignore)))                 ;Ignore loading pictures
     (if (and nroff col)
         (cons "\\.[1-9]$"
               (concat nroff " -man"
                       " %s | "
                       col " -bx")))
     ;; Pass ZIP pointer to win32 winzip
     (if winzip
         (cons "\\.zip$"
               (function
                ((lambda (arg)
                   (tinyurl-call-process-win32
                    winzip  arg))))))))
  "If URL is filename, then check this table for filter.
The `%s' is substituted with the URL (filename) in SHELL-COMMAND string.

If there is Lisp FUNCTION, then it is called with argument URL.

Format:

  '((REGEXP . SHELL-COMMAND)      ;; nil element also accepted
    (REGEXP . FUNCTION)
    ..)

Example:

  The default value for this variable is set like this. If you
  have executables `xv' and `catdoc', then the shell commands are
  defined. If you don't have, then the slot if filled with nil,
  which is acceptable value. The Picture file handler is set to
  `ignore' function, if no `xv' is present to prevent loading
  pictures into Emacs buffer.

 (setq file-filter-table
       (list
        (if (executable-find \"catdoc\")
            '(\"\\\\.doc$\"  . \"catdoc %s\"))  ;View MS WORD files
        (if (executable-find \"xv\")
            '(\".\\\\(jpg\\\\|jpeg\\\\|gif\\\\)$\" .  \"xv %s\")
          'ignore)))"
  :type '(repeat
          (list regexp (choice
                        (string   :tag "Shell command")
                        (function :tag "Function"))))
  :group  'Tinyurl)

(defcustom tinyurl--url-handler-function 'tinyurl-handler-main
  "Function to take care of delegating the URL to correct Agent.
The default function `tinyurl--command-table' uses `tinyurl--command-table'

Function call arguments:
  string      a possible url
  type        :optional A symbol describing url type. See `tinyurl-type'"
  :type  'function
  :group 'TinyUrl)

;;  This variable is set in `tinyurl-install'.

(defcustom tinyurl--command-table nil
  "*What Agent to run when URL is beeing dispatched.
This table cab have multiple different Agent-tables and the currently
used table is stored at `tinyurl--command-table'. See command
\\[tinyurl-set-handler].

The elements:

  TYPE      can be 'mail 'url 'file or 'other. These are the types that
            trigger calling VALUE as function. There is special type name
            'overlay-plist which is used for displaying the overlay.
            Refer to function `tinyurl-type' for all possible TYPE values.

  FUNCTION  Either function or value. Functions are called interactively.

Format:

 '((COMPLETION-NAME
    (
     (TYPE . VALUE)
     ..
     (overlay-plist (PROPERTY VAL PROPERTY VAL ..)))))

References:

  You can contruct one entry to this table with
  functions `tinyurl-command-table-put' `tinyurl-command-table-put-2nd'
  and `tinyurl-command-table-default-1'. See tinyurl.el's source code and
  function `tinyurl-command-table-netscape' how to use these."
  :type  'sexp
  :group 'TinyUrl)

(defcustom tinyurl--email-regexp
  ;; It's best to require some more characters to avoid mishits.
  ;; There is always ".com" ".fi", at least three characters.
  (let ((word  "[^ \t\r\n,:!?%@|'#&]"))
    (concat "<" word "+@" word "+\\." word word word "?>"))
  "Regexp to match email address approximately."
  :type  'string
  :group 'TinyUrl)

(defcustom tinyurl--post-command-hook-threshold 25
  "How often `tinyurl-mark-process-post-command' run after post command.
This variable is used only if funtion `run-with-idle-timer' does
not exist. If the value is 1, then function `tinyurl-mark-process-post-command'
runs after each keypress. You should keep the value in range 10 .. 30,
depending on how quickly you want the process to scan the line for url
buttons."
  :type  'integer
  :group 'TinyUrl)

(defcustom tinyurl--url-cache-buffer  "*URL-cache*"
  "Where to store urls when Emacs is disconnected from the Net."
  :type 'string
  :group  'TinyUrl)

(defcustom tinyurl--reject-url-regexp
  (concat
   ;; "/\\(usr\\|opt\\)\\(/local\\|/ucb\\)?/s?bin"
   ;; "\\|^/bin\\|/dev/"
   "\\.\\(exe\\|com\\|o\\)$")
  "Rgexp to reject URL. This is only used if URL is of type `file'."
  :type  'regexp
  :group 'TinyUrl)

;;}}}
;;{{{ setup: private

;;; ......................................................... &private ...

(defvar tinyurl--mode-manually-turned-off nil
  "On/Off mark when `tinyurl-mode-1' has been changed interactively.")

(make-variable-buffer-local 'tinyurl--mode-manually-turned-off)

;; you can adjust this to include some more character, but please
;; send message to maintainer if you do so.
;;
;; _ $ % & = are many times used in Message-ID's

(defvar tinyurl--cleaner-regexp "[^+~:/?()#%&=_$@.a-zA-Z0-9-]+"
  "When reading the url from buffer, delete characters matching in this regexp.
After cleaning, we should have ready URL.")

(defvar tinyurl--command-table-current nil
  "The active command table name.")

(defvar tinyurl--event nil
  "Last mouse event.")

(defvar tinyurl--timer-elt nil
  "Timer element.")

(defvar tinyurl--history  nil
  "Url history.")

(defvar tinyurl--mouse-yank-at-point nil ;; mouse-yank-at-point
  "Point used when url is clicked.
If nil, when you click on point, the line is immediately
scanned for urls and if the there was url under mouse point, then url
will be followed. If there was no url then call original mouse binding.

If non-nil, The mouse-point is not scanned for urls. Only existing
overlays under point are read.

In short: the t gives the usual 'run marked urls only' and t will say
'install buttins to line, run url at point where the click happened if
there was url'")

;;      Keyboard user's want to see the highlight immediately, so
;;      a 'face setting is better than the 'mouse-face, which is only
;;      seen when mouse is waved over the URL. 'face is immediately
;;      shown in the line.

(defcustom tinyurl--overlay-plist
  (let ((face (if (ti::compat-window-system)
		  'mouse-face
		'face)))
    (if (ti::emacs-p)
        (list
         'rear-nonsticky   t
         'rear-sticky      nil
         'priority         1
         face              'highlight
         'before-string    "!"
         'url              t
         'owner            'tinyurl)
      (list
       'rear-nonsticky    t
       'rear-sticky       nil
       'priority          1
       face               'highlight
       'begin-glyph       (ti::funcall 'make-glyph "!")
       'url               t
       'owner             'tinyurl)))
  "*Property list (PROP VAL PROP VAL ..) used for all overlays."
  :type  'sexp
  :group 'TinyUrl)

(defvar tinyurl--win32-shell-execute-helper
  (when (ti::win32-p)
    (or (and (fboundp 'w32-shell-execute) ;; Emacs
             'w32-shell-execute)
        (and (fboundp 'mswindows-shell-execute) ;; XEmacs
             'mswindows-shell-execute)
        (executable-find "shellex")     ;; Newer Emacs.
        (executable-find "shellex.exe") ;; Emacs 20.2 does not check .exe
        (error "\
** TinyUrl: Automatic setup failed. See ´tinyurl--win32-shell-execute-helper'.
Can't find 'shellex' along `exec-path' with function `executable-find'.
Visit http://www.tertius.com/projects/library/ and get shellex.exe")))
  "*Win32 program or Emacs function to launch native Win32 programs.")

;;}}}
;;{{{ mode and install

;;;###autoload (autoload 'tinyurl-debug-toggle "tinyurl" "" t)

(eval-and-compile (ti::macrof-debug-standard "tinyurl" "--"))

;;; .......................................................... &v-mode ...

;;;###autoload (autoload 'tinyurl-mode            "tinyurl" "" t)
;;;###autoload (autoload 'turn-on-tinyurl-mode    "tinyurl" "" t)
;;;###autoload (autoload 'turn-off-tinyurl-mode   "tinyurl" "" t)
;;;###autoload (autoload 'tinyurl-commentary      "tinyurl" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinyurl-" " U" nil  "Url" 'TinyUrl "tinyurl--"
   "Mark URLs buttons on the line and call appropriate url handlers.

To read the complete documentation, run `tinyurl-commentary'
See also `tinyurl-version' (use prefix argument to see only version number).

Defined keys:

\\{tinyurl--mode-map}"

   "Url mode"
   (progn                              ;Some mode specific things? No?
     (tinyurl-modeline-update)
     (cond
      (tinyurl-mode
       (put 'tinyurl-mode 'global t)
       (add-hook (ti::compat-find-file-hook) 'tinyurl-find-file-hook))
      (t
       (put 'tinyurl-mode 'global nil)
       (remove-hook (ti::compat-find-file-hook) 'tinyurl-find-file-hook)))
     (when (null (get 'tinyurl-mode 'self-call))
       (tinyurl-mode-action tinyurl-mode verb)))
   ;;  The Menubar item takes space and is not useful at least not
   ;;  now, because there is no other functionality in this mode.
   nil
   nil
;;;  "Tiny URL mode"
;;;  (list                                      ;arg 10
;;;   tinyurl:mode-easymenu-name
;;;   ["Find url or call original key ESC RET" tinyurl-key-binding-default t]
;;;   ["Mode help"                             tinyurl-mode-help                 t]
;;;   )
   (progn
     ;;  No, there is no key for `tinyurl-set-handler'. We try to
     ;;  minimize the used keys in this minor mode. Call M-x
     ;;  tinyurl-set-handler if you need to change this (not likely in
     ;;  Non-windowed Emacs)
     (cond
      ((ti::emacs-p)
       (define-key root-map [?\e mouse-2]        'tinyurl-set-handler)
       ;;  We have to define this, because widget.el uses down-mouse-2
       ;;  and we must see it first.
       (define-key root-map [down-mouse-2]       'tinyurl-mouse-binding-down)
       (define-key root-map [mouse-2]            'tinyurl-mouse-binding))
      (t
       (define-key root-map [(meta button2)]     'tinyurl-set-handler)
       (define-key root-map [(button2)]          'tinyurl-mouse-binding)))
     (define-key root-map "\e\C-m" 'tinyurl-key-binding-default))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mode-turn-on-ok-p ()
  "Check if 'tinyurl-mode-1' is allowed to be turned on for the buffer.
The buffer is seached for basic URL references and checked against
`tinyurl--exclude-function'."
  (and (null tinyurl-mode)
       (or  (null tinyurl--exclude-function)
            (null (funcall tinyurl--exclude-function (current-buffer))))
       (ti::re-search-check
        (concat "\\(ftp\\|https?\\)://"
                "\\|<[^ \t\n]+@[^ \t\n]+>"
                "\\|mailto:[^ \t\n]+@[^ \t\n]+"))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-turn-off-tinyurl-mode-1-maybe ()
  "Activate or deactivate `tinyurl-mode-1' in current buffer.
Try to find ftp, http or email URL.
The value of `tinyurl--exclude-function' is consulted first."
  (if (tinyurl-mode-turn-on-ok-p)
      (turn-on-tinyurl-mode-1)
    (turn-off-tinyurl-mode-1)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinyurl-mode-1-maybe ()
  "Activate `tinyurl-mode-1' in current buffer if ftp, http or email is found.
This function is meant to be used in e.g. Article display
hooks in Mail Agents.

References:

  The value of `tinyurl--exclude-function' is consulted first."
  (when (tinyurl-mode-turn-on-ok-p)
    (turn-on-tinyurl-mode-1)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinyurl-mode-mail ()
  "Turn on  `tinyurl-mode-1' and make `tinyurl--mouse-yank-at-point' local."
  (make-local-variable 'tinyurl--mouse-yank-at-point)
  ;;  We set this to t, so that clicking url means scanning line
  ;;  immediately.
  (setq tinyurl--mouse-yank-at-point t)
  (unless tinyurl-mode
    (turn-on-tinyurl-mode-1)))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinyurl-mode-automatically ()
  "This function is called from idle timer process `tinyurl-mark-process'.
If `tinyurl--mode-global-turned-off' is set, do nothing."
  (when (and (get  'tinyurl-mode 'global)
	     (boundp 'tinyurl--mode-global-turned-off)
             (null tinyurl--mode-global-turned-off)
             (tinyurl-mode-turn-on-ok-p))
    (turn-on-tinyurl-mode-1)
    t))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinyurl-mode-1 ()
  "Turn URL mode on for this buffer only."
  (interactive)
  (unless tinyurl-mode
    (when (interactive-p)
      (setq tinyurl--mode-manually-turned-off nil))
    (tinyurl-mode-1 1)))

;;; ----------------------------------------------------------------------
;;;
(defun turn-off-tinyurl-mode-1 ()
  "Turn URL mode off for this buffer only."
  (interactive)
  (when tinyurl-mode
    (when (interactive-p)
      (setq tinyurl--mode-manually-turned-off t))
    (tinyurl-mode-1 0)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-overlay-kill  ()
  "Kill used overlays.
This function only kills overlays recoded to internal list.
Thje internal list may be inaccurate an to definitely wipe out
TinyUrl overlays, use `tinyurl-overlay-kill-in-buffer'."
  (put 'tinyurl-mark-line 'point nil)
  (dolist (ov (get 'tinyurl-mark-line 'ov-list))
    (delete-overlay ov)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-overlay-kill-in-buffer ()
  "Kill TinyUrl overlays from whole buffer. See also `tinyurl-overlay-kill'."
  (interactive)
  (put 'tinyurl-mark-line 'point nil)
  (ti::overlay-remove-region
   (point-min)
   (point-max)
   '(owner tinyurl)
   'prop-val-list))

;;; ----------------------------------------------------------------------
;;;###autoload
(defun tinyurl-mode-1 (arg)
  "Turn mode on or off with mode ARG for current buffer only.
If you want to turn on or off globally, use function `tinyurl-mode'."
  (interactive "P")
  (unless (assq 'tinyurl-mode minor-mode-map-alist)
    (tinyurl-install-mode))
  (ti::bool-toggle tinyurl-mode arg)
  (tinyurl-modeline-update)
  (unless tinyurl-mode                  ;Cleanup overlays on exit
    (tinyurl-overlay-kill-in-buffer)
    (tinyurl-overlay-kill))
  (when (interactive-p)
    (setq tinyurl--mode-manually-turned-off (not tinyurl-mode)))
  tinyurl-mode)

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mode-action (&optional mode verb)
  "Turn MODE `tinyurl-mode' on or off everywhere. See `tinyurl-mode'.
This function must not be called directly, not even from Lisp. Use
function `tinyurl-mode' function instead. VERB."
  (unless (get 'tinyurl-mode 'self-call)
    (run-hooks 'tinyurl--mode-define-keys-hook))
  (let ((i 0)
	tinyurl--mode-define-keys-hook)
    (unwind-protect
        (progn
          ;;  Raise the flag to prevent calling us
          (put 'tinyurl-mode 'self-call t)
          ;;  For every buffer, either turn mode on or off.
          (dolist (buffer (buffer-list))
            (incf  i)
            ;;  Exclude hidden buffers
            (when (not (string-match "^ " (buffer-name buffer)))
              (with-current-buffer buffer
                (cond
                 (mode
                  ;;  Mark all buffers as "not modified"
                  (setq tinyurl--mode-manually-turned-off nil)
                  (turn-on-tinyurl-mode-1-maybe))
                 (t
                  (turn-off-tinyurl-mode)
                  (setq tinyurl--mode-manually-turned-off t)))))))
      (when verb
        (message "TinyUrl: Global mode is %s. Stepped through %d buffers"
                 (if mode
                     "on"
                   "off")
                 i)
        (sit-for 1))
      (put 'tinyurl-mode 'self-call nil))))

;;; ----------------------------------------------------------------------
;;;###autoload
(defun tinyurl-install (&optional uninstall)
  "Install or `UNINSTALL package."
  (interactive "P")
  (put 'tinyurl-plugged-p 'mode nil)
  (ti::compat-timer-cancel-function 'tinyurl-mark-process)
  (tinyurl-install-mode)
  (ti::add-hooks '(Man-mode-hook
                   compilation-mode-hook)
                 'turn-on-tinyurl-mode-1
                 uninstall)
  (remove-hook 'post-command-hook 'tinyurl-mark-process-post-command)
  (tinyurl-install-command-table)
  ;;  If the idle timer is available, use it. Otherwise we would have
  ;;  no other option but occupy post command hook
  (unless uninstall
    (if (ti::idle-timer-supported-p)
        (setq tinyurl--timer-elt
              (ti::funcall 'run-with-idle-timer 2 t 'tinyurl-mark-process))
      (add-hook 'post-command-hook
                'tinyurl-mark-process-post-command))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-install-to-packages (&optional restore-original)
  "Make TinyUrl default top level url handler: GNUS, TM, VM etc.
Optionally RESTORE-ORIGINAL url handlers."
  (interactive "P")
  (let ()
;;;         (list '(gnus-button-url
;;;                 gnus-button-embedded-url
;;;                 tm:browse-url
;;;                 vm-mouse-send-url
;;;                 ))
    (ti::add-hooks '(rmail-show-message-hook
                     vm-select-message-hook
                     mh-show-mode-hook)
                   'turn-on-tinyurl-mode-mail
                   restore-original)
    ;; Using advice
    ;; 1) package may not be loaded yet, advice activated when it loads.
    ;; 2) Changing the MUA varibles would maen requiring the feature,
    ;;    and then changing the defaults, but what guarrantees that user
    ;;    doesn't reset the vars somewhere else?
    ;; 3) Gnus adds all button to the article, but tinyurl only looks
    ;;    current line
    ;; See gnus-art.el
    ;; gnus-button-embedded-url gnus-button-url gnus-url-mailto
    (when nil                           ;Enabled now
      (require 'advice)
      (defadvice gnus-button-url (around tinyurl dis)
        "Replace function and call `tinyurl--url-handler-function'"
        (let ((URL  (ti::string-remove-whitespace (ad-get-arg 0))))
          (funcall tinyurl--url-handler-function URL)))
      (defadvice gnus-article-push-button (around tinyurl dis)
        "Replace function and call `tinyurl--url-handler-function'"
        (let ((URL  (ti::string-remove-whitespace (ad-get-arg 0))))
          (funcall tinyurl--url-handler-function URL)))
      (defadvice gnus-button-embedded-url (around tinyurl dis)
        "Replace function and call `tinyurl--url-handler-function'"
        (let ((URL  (ti::string-remove-whitespace (ad-get-arg 0))))
          (funcall tinyurl--url-handler-function URL)))
      (defadvice gnus-url-mailto (around tinyurl dis)
        "Replace function and call `tinyurl--url-handler-function'"
        (let ((URL  (ti::string-remove-whitespace (ad-get-arg 0))))
          (funcall tinyurl--url-handler-function URL)))
      ;;  vm-mouse-send-url (url &optional browser)
      (defadvice vm-mouse-send-url (around tinyurl act)
        "Replace function and call `tinyurl--url-handler-function'"
        (funcall tinyurl--url-handler-function (ad-get-arg 0))))
    ;;  TM/SEMI Unfortunately has no hook that runs when preview
    ;;  buffer is created with
    (when nil
      (require 'advice)
      (defadvice tm:browse-url  (around tinyurl act) ;TM.el
        "Replace function and call `tinyurl--url-handler-function'"
        (tinyurl-at-point 'verb))
      (defadvice mime-viewer/make-preview-buffer (after tinyurl act)
        "Call `turn-on-tinyurl-mode-mail'."
        (turn-on-tinyurl-mode-mail))
      (defadvice mime-edit-preview-message  (after tinyurl act)
        "Call `turn-on-tinyurl-mode-mail'."
        (turn-on-tinyurl-mode-mail))
      (if restore-original
          (ti::advice-control list "tinyurl" 'disable)
        (ti::advice-control list "tinyurl")))
    (when (ti::win32-p)
      (defconst gnus-button-url 'tinyurl-dispatcher-1) ; GNUS
      ;; VM
      (defconst vm-url-browser 'tinyurl-dispatcher-1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-file-hook ()
  "Turn on tinyurl mode if `tinyurl--mode-global' is non-nil."
  (when (and (get 'tinyurl-mode 'global)
             (null tinyurl-mode))
    (turn-on-tinyurl-mode-1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-set-handler (table)
  "Set active url handler command TABLE."
  (interactive
   (list (completing-read
          "TinyUrl, use command table: " tinyurl--command-table nil t)))
  (setq tinyurl--command-table-current table)
  (tinyurl-modeline-update))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-plugged-p ()
  "Return plugged status."
  (or (get 'tinyurl-plugged-p 'mode)
      (ti::mail-plugged-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-plugged-always-p ()
  "Return true plugged status."
  t)

;;; ----------------------------------------------------------------------
;;; Called by the Line marker process to keep track of the Gnus mode changes
;;;
(defun tinyurl-plugged-update ()
  "Update plugged status."
  (put 'tinyurl-plugged-p 'mode (tinyurl-plugged-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-plugged-mode-toggle (&optional arg verb)
  "Set plugged status according to ARG. 1 means plugged and 0 unplugged.
When mode is nil, assume unplugged unless `ti::mail-plugged-p'
\(Gnus) says otherwise."
  (interactive "P")
  (let ((mode (get 'tinyurl-plugged-p 'mode)))
    (ti::verb)
    (ti::bool-toggle mode arg)
    (put 'tinyurl-plugged-p 'mode mode)
    (if verb
        (message "TinyUrl: %s"
                 (if mode "Plugged" "Unplugged")))

    (if (boundp 'gnus-plugged)
        (setq gnus-plugged mode))

    (tinyurl-modeline-update)
    (get 'tinyurl-plugged-p 'mode)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-call-process-win32 (binary &rest args)
  "Call Win32 native BINARY with ARGS"
  (interactive)
  (cond
   ((stringp tinyurl--win32-shell-execute-helper)
    (apply 'call-process
           tinyurl--win32-shell-execute-helper
           nil
           nil
           nil
           ;; binary
           args))
   ((functionp tinyurl--win32-shell-execute-helper)
    (apply tinyurl--win32-shell-execute-helper "open" args))
   (t
    (message
     "TinyUrl: `tinyurl--win32-shell-execute-helper' not configured."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-default-exclude  (buffer)
  "Default function for `tinyurl--exclude-function' to ignore BUFFER.
Ignores VM, W3, DIRED, ARCHIVE, COMPILE, GREP buffers.

The buffer ignore status is recorded to the plist of
function ´tinyurl-default-exclude', which you can recall with:

  (get 'tinyurl-default-exclude 'exclude-list)"
  (with-current-buffer buffer
    (let* ((exclude-list  (get 'tinyurl-default-exclude
                               'exclude-list))
           (nok-status    (assq buffer exclude-list)))
      (unless nok-status
        (let* ((name (symbol-name major-mode))
               (stat (string-match
                      (concat
                       "^w3-\\|^vm-\\|dired\\|archive\\|compil\\|grep$"
                       "\\|archive")
                      name)))
          (when stat
            (pushnew (cons buffer 'exclude) exclude-list :test 'equal)
            (put 'tinyurl-default-exclude 'exclude-list exclude-list)
            (message "TinyUrl: Excluded buffer ´%s' Major-mode: %s"
                     (buffer-name)
                     name)
            stat))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-put (table key value)
  "Use command TABLE entry and change KEY's value to new VALUE."
  (let (elt
	new
	ok)
    (while (setq elt (pop table))
      (when (eq (car elt) key)
        (setq elt (cons key value)
              ok  t))
      (push elt new))
    (or ok
        (error "TinyUrl: No key %s found" key))
    (nreverse new)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-put-2nd (table key1 key2 value)
  "Use command TABLE, find KEY1, change 2nd level KEY2's value to new VALUE.
The TABLE is modified in place."
  (let (elt
	list
	mem
	new
	ok)
    (or (setq elt (assq key1 table))
        (error "TinyUrl: Key1 %s does not exist" key1))
    (setq list (nth 1 elt))
    (while list
      (setq mem (car list))
      (push mem new)
      (when (eq mem key2)
        ;;   Raise flag, change value
        (setq ok t)
        (push value new)
        ;; skip next element, because this is the old value.
        (setq list (cdr list)))
      (setq list (cdr list)))
    (setq new (nreverse new))
    (unless ok
      (error "No key2 '%s'" key2))

    (setcdr elt (list new))            ; Change key1's right hand list
    table))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-browse-url-browser-function ()
  "Return default `browse-url-browser-function'."
  (when (and (boundp 'browse-url-browser-function)
             browse-url-browser-function)
    ;; If the value is not a function it should be a list of pairs
    ;; (REGEXP . FUNCTION)
    (cond
     ((functionp browse-url-browser-function)
      (if (not (eq browse-url-browser-function
                   'tinyurl-dispatcher-1))
          browse-url-browser-function))
     ((listp browse-url-browser-function)
      (dolist (elt browse-url-browser-function)
        (when (string-match "netscape" (symbol-name (cdr-safe elt)))
          (return (cdr elt))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-user-default-browser-type ()
  "What kind of browser user used before? \"netscape\" or \"iexplore."
  (let* ((browse (tinyurl-browse-url-browser-function))
         (user-default
          (when browse
            (cond
             ((string-match "netscape" (symbol-name browse))
              'netscape)
             ((ti::win32-p)
              'iexplore)
             (t
              ;;  Don't know. Use whatever is there.
              'default)))))
    user-default))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-user-command-table-default ()
  "Return default command table choice.
This might be \"netscape\" or \"iexplore\"."
  (interactive)
  (let ((default (tinyurl-user-default-browser-type)))
    (cond
     ((ti::win32-p)                     ;Win32
      (let ((net (executable-find "netscape")))
        (if (and net
                 (eq default 'netscape))
            "netscape"
          ;; "c:/Program Files/Internet Explorer/iexplore.exe
          "iexplore")))
     (t                                 ;Unix
      (if (not (ti::compat-window-system))
          "w3"
        ;;  In Unix the name has "r" at the end
        (let ((ie (executable-find "iexplorer")))
          (cond
           (ie
            "iexplore")
           ((eq default 'netscape)
            "netscape")
           (t
            "default"))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-default-1 ()
  "Return default `tinyurl--command-table' entry.
If you change this function's source, run

  (tinyurl-install-command-table 'force)

References:
 `tinyurl--overlay-plist'"
  (list
   ;; could also use `tinyurl-find-url-mail'
   '(mail               . tinyurl-find-url-mail-browse-url)
   '(url-message-id     . tinyurl-find-url-message-id)
   '(url-http           . tinyurl-find-url-http)  ;; www.x.com
   '(url-ftp            . tinyurl-find-url-file)  ;; ftp://site.com
   '(url                . browse-url)
   '(url-ange           . find-file) ;; /foo@site.com:
   '(url-debian-bts     . tinyurl-find-debian-bts-bug) ;; Bug#NNNNNN
   '(file               . tinyurl-find-url-file)
   '(file-packed        . tinyurl-find-url-file)
   '(file-code-c        . ff-find-other-file)
   '(file-code-lisp     . tinyurl-find-url-lisp)
   '(file-code-perl             . tinyurl-find-url-perl)
   '(file-code-perl-pod-manpage . tinyurl-find-url-perl-pod-manpage)
   '(file-code-perl-pod-module  . tinyurl-find-url-perl-pod-module)
   '(file-code-perl-method      . tinyurl-find-url-perl-method)
   '(compiler-perl-in-file-at-line      . tinyurl-find-url-perl-compile)
   '(compiler-perl-at-line              . tinyurl-find-url-perl-compile)
   '(compiler-php-at-line               . tinyurl-find-url-php-compile)
   '(file-other         . ffap)
   '(man                . tinyurl-find-url-man)
   '(other              . tinyurl-find-url-file)
   (list
    'overlay-plist
    tinyurl--overlay-plist)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyurl-command-table-before-string (entry string)
  "Replace property 'before-string in ENTRY with STRING."
  `(if (ti::emacs-p)
       (tinyurl-command-table-put-2nd
        ,entry
	'overlay-plist
	'before-string
	,string)
     (tinyurl-command-table-put-2nd
      ,entry
      'overlay-plist
      'begin-glyph
      (ti::funcall 'make-glyph ,string))))

;;; ----------------------------------------------------------------------
;;; This is a copy from function `browse-url'.
(defun tinyurl-command-browse-url-default-browser-function-1
  (&optional url)
  "Return function from `browse-url-browser-function' for URL.
URL defaults to http"
  (when (boundp 'browse-url-browser-function)
    (if (functionp browse-url-browser-function)
        browse-url-browser-function
      ;; The `function' can be an alist; look down it for first match
      ;; and apply the function (which might be a lambda).
      (dolist (elt browse-url-browser-function)
        (when (string-match (car elt) (or url "http"))
          (return (cdr elt)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-browse-url-default-browser-function ()
  "Find brose-url.el function that would  call the default broser.
If not found, signal error."
  (or (tinyurl-command-browse-url-default-browser-function-1)
      (error "TinyUrl: `browse-url-browser-function' is not set.
Has brose-url.el been loaded?")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-default ()
  "Return \"default\" entry."
  (let ((entry (tinyurl-command-table-default-1)))
    ;; (setq entry (tinyurl-command-table-put
    ;;              entry
    ;;              'url
    ;;              (tinyurl-command-browse-url-default-browser-function)))
    (tinyurl-command-table-before-string entry "!")
    entry))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-netscape ()
  "Return Netscape entry."
  (let ((entry (tinyurl-command-table-default-1)))
    (when (executable-find "netscape")
      (setq entry (tinyurl-command-table-put
                   entry 'url 'browse-url-netscape))
      (tinyurl-command-table-before-string entry "!")
      entry)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-iexplore ()
  "Return Unix Iexplorer entry."
  (let ((entry (tinyurl-command-table-default-1)))
    (when (executable-find "iexplorer") ;; Extra "r" in name
      (setq entry (tinyurl-command-table-put
                   entry 'url 'browse-url-iexplore))
      (tinyurl-command-table-before-string entry "!")
      entry)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-netscape-win32 ()
  "Return Netscape entry."
  (let ((entry (tinyurl-command-table-default-1)))
    (setq entry (tinyurl-command-table-put
                 entry 'url 'tinyurl-find-url-win32-netscape))
    (tinyurl-command-table-before-string entry "!")
    entry))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-iexplore-win32 ()
  "Return Netscape entry."
  (let ((entry (tinyurl-command-table-default-1)))
    (setq entry (tinyurl-command-table-put
                 entry 'url 'tinyurl-find-url-win32-iexplore))
    (tinyurl-command-table-before-string entry "@")
    entry))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-lynx ()
  "Return Lynx entry."
  (let ((entry (tinyurl-command-table-default-1)))
    (setq entry (tinyurl-command-table-put
                 entry 'url 'browse-url-lynx-emacs))
    (tinyurl-command-table-before-string entry "*")
    entry))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-w3 ()
  "Return W3 entry."
  (let ((entry (tinyurl-command-table-default-1)))
    (setq entry (tinyurl-command-table-put
                 entry 'url 'browse-url-w3))
    (tinyurl-command-table-before-string entry "?")
    entry))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-defaults ()
  "Return default value for `tinyurl--command-table'."
  (delq nil                             ;remove empty entries
        (list
         (list "default" (tinyurl-command-table-default))
         (if (ti::win32-p)
             (list "netscape" (tinyurl-command-table-netscape-win32))
           (list "netscape" (tinyurl-command-table-netscape)))
         (if (ti::win32-p)
             (list "iexplore" (tinyurl-command-table-iexplore-win32))
           (list "iexplore" (tinyurl-command-table-iexplore)))
         ;; FIXME: Ahem, there is Lynx for Win32; but I don't know
         ;; if anybody uses it.
         (unless (ti::win32-p)
           (list "lynx"  (tinyurl-command-table-lynx)))
         (list "w3"    (tinyurl-command-table-w3)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-command-table-defaults-set ()
  "Set `tinyurl--command-table' to defaults."
  (interactive)
  (setq tinyurl--command-table (tinyurl-command-table-defaults)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-install-command-table (&optional force)
  "Set default values to `tinyurl--command-table'. FORCE reset."
  (if force
      (tinyurl-command-table-defaults-set)
    (or tinyurl--command-table
        (tinyurl-command-table-defaults-set)))
  ;;  Some safety measure needed..
  (unless (stringp (caar tinyurl--command-table))
    (error "TinyUrl: Setting `tinyurl--command-table' failed."))
  ;;  Set default only if it is NIL
  (unless (stringp tinyurl--command-table-current)
    (setq tinyurl--command-table-current
          (tinyurl-user-command-table-default))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-modeline-update ()
  "Update modeline name."
  (tinyurl-install-command-table)
  (tinyurl-plugged-update)
  (setq tinyurl--mode-name
        (concat " U"
                (downcase (ti::string-left tinyurl--command-table-current 1))
                (if (funcall tinyurl--plugged-function)
                    "!" "")))
  (ti::compat-modeline-update))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-set-mouse-maybe (event)
  "Set point to mouse EVENT and mark URLs in the line."
  (when (and event (null tinyurl--mouse-yank-at-point))
    (goto-char (ti::mouse-point event))
    (tinyurl-mark-line)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-table (table)
  "Return command TABLE."
  (or (nth 1 (assoc table tinyurl--command-table))
      (prog1 nil)))
        ;; (message "TinyUrl: [ERROR] No such command table: [%s] " table)

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-table-current (&optional table)
  "Return copy of active command table.
References:
  `tinyurl--display-glyph'"
  (let ((table (tinyurl-table
		(or table
		    tinyurl--command-table-current))))
    (when (and table
               (null tinyurl--display-glyph))
      ;;  Make local copy and changing `before-string' to ""
      (setq table (copy-tree table))
      (setcar (nthcdr 1 (member
                         (if (ti::emacs-p)
                             'before-string
                           'begin-glyph)
                         (nth 1 (assq 'overlay-plist table))))
              (if (ti::emacs-p)
                  ""
                (ti::funcall 'make-glyph ""))))
    table))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-overlay-plist (&optional table)
  "Return overlay plist of TABLE."
  (nth 1 (assq 'overlay-plist (tinyurl-table-current table))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-agent-function (type)
  "Return agent function for TYPE. See `tinyurl--command-table'."
  (let ((elt (cdr (assq type (tinyurl-table-current)))))
    (unless elt
      (error "Tinyurl: Unknown type %s" type))
    (tinyurl-debug "tinyurl-agent-function" elt)
    elt))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-agent-funcall (type url)
  "Call correct function according to TYPE and pass it an URL."
  (funcall (tinyurl-agent-function type) url))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-types ()
  "Return known url types in `tinyurl--command-table-current'."
  (mapcar 'car (tinyurl-table-current)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mouse-binding (event)
  "Jump to URL at point or call original function with mouse EVENT."
  (interactive "e")
  (setq tinyurl--event event)
  (tinyurl-dispatcher event 'mouse))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mouse-binding-down (event)
  "Jump to URL at point or call original function with mouse EVENT."
  (interactive "e")
  (setq tinyurl--event event)
  (put 'tinyurl--event 'down-event event))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-key-binding-default ()
  "Jump to URL at point or call original ESC RET key binding."
  (interactive)
  (setq tinyurl--event nil)
  (tinyurl-mark-line)
  (tinyurl-dispatcher "\e\C-m" 'key))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-overlay-get (&optional point)
  "Reeturn overlay from current POINT if there is any '(owner tinyurl)."
  (let ((list (overlays-at (or point (point)))))
    (if list
        (ti::overlay-get-prop list '(owner tinyurl)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyurl-get-filter (url)
  "Return filter or any for URL."
  (cdr-safe (ti::list-find (delq nil tinyurl--file-filter-table) url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-filter-pod (url)
  "Handle Perl pod URL."
  (let ((pod   (or (get 'tinyurl-filter-pod 'pod2text)
		   (executable-find "pod2text")
		   (message "TinyUrl: No `pod2text' command found.")
		   nil))
	(buffer  shell-command-output-buffer))
    (put 'tinyurl-filter-pod 'pod2text pod)
    (if (null pod)
        (find-file url)
      (call-process pod nil buffer nil url)
      (when (and (get-buffer buffer)
                 (featurep 'tinyperl))
        (with-current-buffer buffer
          (turn-on-tinyperl-pod-view-mode))
        (ti::pop-to-buffer-or-window buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-dispatch-ignore-p  (&rest dummy)
  "Check if control is passed back to underlying mode. Ignore DUMMY."
  (memq major-mode '(archive-mode
                     dired-mode
                     dired-virtual-mode
                     tar-mode
                     zip-mode)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-dispatcher-1  (url)
  "Redirect URL to proper agent handler."
  (interactive)
  (let ((fid                       "tinyurl-dispatcher-1:")
	(tinyurl--file-filter-table  tinyurl--file-filter-table) ;; make copy
	url-type
	tmp
	ret)
    ;; ....................................................... do-it ...
    (cond
     ((and tinyurl--validate-hook
           (not (eq t (setq ret (run-hook-with-args-until-success
                                 'tinyurl--validate-hook url)))))
      (if (not (stringp ret))
          (message "TinyUrl: url ignored. See tinyurl:-url-validate-hook")
        (message ret)))
     (t ;; it's ok
      (when current-prefix-arg
        (setq url (read-from-minibuffer
                   "(TinyUrl) edit: "
                   url
                   nil
                   nil
                   'tinyurl--history))
        ;; User can also control the access method, But beware.
        ;; Trying to call Url that is not Perl type is disastrous
        (unless (ti::nil-p url)
          (setq url-type
                (intern
                 (completing-read
                  "(TinyUrl) Select type: "
                  (ti::list-to-assoc-menu
                   (mapcar 'symbol-name (tinyurl-types)))
                  nil
                  'match-needed
                  (if (tinyurl-type url)
                      (symbol-name (tinyurl-type url))
                    nil)))))
        (when (and (setq tmp (tinyurl-get-filter url))
                   (y-or-n-p
                    (format "(TinyUrl) By-pass filter [%s]? "
                            (prin1-to-string tmp))))
          (setq tinyurl--file-filter-table nil))) ;; when
      (tinyurl-debug fid url-type tinyurl--url-handler-function url)
      (if (not (ti::nil-p url))
          (funcall tinyurl--url-handler-function url url-type))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-gnus-callback-at-point ()
  "Return gnus-callback text property at point."
  (get-text-property (point) 'gnus-callback))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-call-original-mouse (event)
  "Call original mouse-2 function, unless in compilation buffer."
  (let* ((mode (symbol-name major-mode))
         (function
          (let ((local (current-local-map))
		tinyurl-mode)
            (or (and local
                     (if (ti::emacs-p)
                         (lookup-key local [mouse-2])
                       (lookup-key local [(button2)])))
                (if (ti::emacs-p)
                    (lookup-key global-map [mouse-2])
                  (lookup-key global-map [(button2)]))))))
    (if (and (string-match "yank" (symbol-name function))
             (or (string-match "compil" mode) ;compilation, compile
                 buffer-read-only))
        (message "TinyUrl: Nothing to (yank) here.")
      (ti::compat-mouse-call-original 'tinyurl-mode event))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-gnus-data-at-point ()
  "Return gnus-data text property at point."
  (get-text-property (point) 'gnus-data))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-dispatcher (&optional event type)
  "See if there is URL at point. Otherwise act like usual key/mouse call.

Input:

  EVENT     mouse-event or key binding
  TYPE      'mouse or 'key. The EVENT type"

  (let* ((ov     (tinyurl-overlay-get))
         (url    (and ov
                      (ti::overlay-buffer-substring ov 'no-properties)))
         (nok-p  (or (null url)
                     (and tinyurl--dispatch-hook
                          (run-hook-with-args-until-success
                           'tinyurl--dispatch-hook
                           url
                           (cons (current-buffer) (point))))))
         ;; (mouse-2 . gnus-article-push-button)
         ;; (gnus-callback gnus-article-toggle-cited-text)
         (gnus-callback  (tinyurl-gnus-callback-at-point))
         (gnus-data      (tinyurl-gnus-data-at-point)))
    ;;  Notice that if you add text near the overlay, the overlay
    ;;  starts stretching an the beg end point do not accurately
    ;;  designate the URL.
    ;;
    ;;  Also see this example url that may be in quotes, "ftp://foo.com/"
    ;;  or surrounded by parenthesis, whatever. We remove invalid
    ;;  characters. The "#" must stay, ebacsue it's NAME tag inside URL
    ;;
    ;;    ftp://foo.com/this.txt#tag
    ;;    ftp://foo.com/perl.pl?params

    (when (eq type 'mouse)
      (tinyurl-set-mouse-maybe event))
    (cond
     (nok-p
      (cond
       ((eq type 'mouse)
        ;;  The underlying application may have defined down-event; like
        ;;  widget.el does in Gnus. In that case; we must give priority
        ;;  to down-event. Otherwise call normal mouse-2 event.
        (let* ((down-event (get 'tinyurl--event 'down-event))
               (down-func  (if down-event
                               (ti::compat-mouse-call-original-function
                                'tinyurl-mode
                                down-event))))
          ;;  Now clear events, so that these old ones are not used.
          (setq tinyurl--event nil)
          (put 'tinyurl--event 'down-event nil)
          (cond
           (gnus-callback
            (funcall gnus-callback gnus-data))
           ((and down-func
                 (fboundp down-func))
            (tinyurl-call-original-mouse down-event))
           (t
            (tinyurl-call-original-mouse event)))))
       (t
        (ti::compat-key-call-original 'tinyurl-mode event))))
     (t
      (tinyurl-dispatcher-1 url)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-at-point (&optional verb)
  "Mark line for urls and go to the url at point if any. VERB."
  (interactive)
  (ti::verb)
  (tinyurl-mark-line)
  (let* ((ov    (tinyurl-overlay-get))
         (URL   (and ov
                     (buffer-substring-no-properties
                      (overlay-start ov) (overlay-end ov)))))
    (cond
     (URL
      (funcall tinyurl--url-handler-function))
     (verb
      (message "TinyUrl: No url found.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-validate-url-default (url)
  "Default URL validate.
- Discard foo|bar|quux urls and character $, like in $THIS_DIR.
- Discard Files that do not exist.
- Discard all /dev or /proc files

Return:

  t         URL accepted
  string    Error Message."

  (let ((fid  "tinyurl-validate-url-default:")
	(info (ti::file-path-and-line-info url)) ;FILE:NBR --> FILE
	(type (tinyurl-type url))
	ret)
    (if info
        (setq url (car info)))
    (setq
     ret
     (cond
      ((string-match "^/\\(dev\\|proc\\)/" url)
       (format "TinyUrl: (url validate) Device file ignored"))
      ((string-match
        (concat
         "^\\(/usr\\(/local\\)?\\|/opt\\|/vol\\)?/s?bin/"
         ;; Ehm. What to do with Windows and Cygwin Files? This is
         ;; an approximation
         "\\|^C:[\\/]win")
        url)
       (format "TinyUrl: (url validate) Binary file ignored"))
      ((ti::file-name-remote-p url)
       t)                               ;do not check ange-ftp
      ((or (string-match "\\<\\(foo\\|bar\\|quux\\)" url)
           (string-match "\\$" url))
       (format "TinyUrl: (url validate) Invalid keyword '%s' in URL [%s]"
               (match-string 0 url)
               url))
      ((and (string-match "^[~/\\]\\|^[a-z]:[/\\]" url)
            ;; Ange is called if file contains :, prevent it
            (not (string-match "^/[a-z]+@[0-9a-z.-]+:" url))
            (not (file-exists-p url)))
       (format "TinyUrl: (url validate) File not found [%s]" url))
      ((and (string-match "file" (symbol-name type))
            (stringp tinyurl--reject-url-regexp)
            (string-match tinyurl--reject-url-regexp url))
       "TinyUrl: (url validate) rejected by `tinyurl--reject-url-regexp'")
      ((and (string-match (or (ti::id-info nil 'cache) "")  "perl")
            (save-excursion
              (beginning-of-line)
              (looking-at
               (concat
                ".*\\("
                "=~\\|!~\\|=!"          ; =~  or !~ =!
                "\\|! *m?/"             ; if ( ! /this/ )
                "\\|if[ \t]+m?/"        ; $1 if /match/
                "\\|=[ \t]+m?/"         ; = m/this/
                "\\|\\<s/"              ; s/this/that
                "\\|\\<qq?/"            ; q/word word word/;
                "\\)"))))
       (concat "TinyUrl: (url validate) Perl like statement rejected: "
               (match-string 1)))
      (t
       t)))
    (tinyurl-debug fid url ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-validate-url-perl-method (url)
  "Check Perl Foo::Bar->new(...)."
  (cond
   ((not (string-match "perl" (ti::id-info)))
    "TinyUrl: (perl url validate) rejected due to non-perl buffer")
   (t
    t))) ;; accept

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-validate-url-email (url)
  "Accept email url only if it doesn't overlap with http://.
E.g. Following url would be targetted as email, because it has <.*@.*>

<URL:http://groups.google.com/groups?as_q=&as_umsgid=3cgd8m0w.fsf@blue.sea.net>"
  (cond
   ((string-match "http://\\|file:/\\|ftp://" url)
    "TinyUrl: (email url validate) rejected due to URI reference: %s" url)
   ((not (string-match
          "[0-9a-z.-]+@[0-9a-z]+\\(\\.[0-9a-z-]+\\)*\\.[a-z]+>?$" url))
    "TinyUrl: (email url validate) does no look like mail address: %s" url)
   (t
    t))) ;; accept

;;}}}
;;{{{ URL handler

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-lisp (url)
  "Find Emacs Llisp package URL."
  (let ((file (ti::string-match "[\"']\\([^\"')]+\\)" 1 url)))
    (when file
      (setq file (replace-regexp-in-string "c$" "" file))
      (setq file (ti::string-verify-ends file "\\.el" ".el")))
    (cond
     ((null file)
      (message "TinyUrl: Odd url %s" url))
     ((null (setq file (locate-library file)))
      (message "TinyUrl: %s  not found from lisp `load-path'" url))
     (t
      (find-file file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-debian-bts-bug (url)
  "Find Debian BTS bug URL."
  (let (bug)
    (when (setq bug (ti::string-match "\\([0-9]+\\)" 1 url))
      (if (eq (length bug) 6)
          (tinydebian-bug-browse-url-by-bug bug)
        (message "TinyUrl: Incorrect bug number %s" bug)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-php-compile (url)
  "Find PHP compiler error URL."
  ;; <b>Parse error</b>:  parse error in <b>FILE.php</b> on line <b>161</b><br>
  (let ((file (ti::string-match
	       "parse error in <b>\\([^<\n]+\\)</b> *on line"
	       1
	       url))
	(line (ti::string-match
	       "parse error in.*on line <b>\\([0-9]+\\)"
	       1
	       url)))
    (if (null line)                   ;Quiet byte compiler: unused var
        (setq line nil))
    ;; FIXME:  Actually the general FILE-FIND URL method already can grab
    ;; the filename and jump to the correct location, so I'm not sure we need
    ;; specific PHP url handler.
    (if file ;; This is no-op, quiet byte compiler for now.
        (setq file file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-perl-pod-manpage (url)
  "Find perl POD manpage URL."
  (setq url (ti::string-match "perl[^] ,.\n\t]+" 0 url))
  (let (point)
    ;; Check if the referenced pod page is on the current buffer
    ;; NAME
    ;;    perlfunc - Perl builtin functions
    ;;
    ;; DESCRIPTION
    (setq point (ti::re-search-check (format "NAME\n +%s -" url)))
    (if point
        (goto-char point)
      (tinyperl-pod-by-manpage (tinyperl-pod-manpage-to-file url)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-perl-pod-module (url)
  "Find perl POD page: URL."
  (setq url (replace-regexp-in-string " +manpage" "" url))
  (tinyperl-pod-by-module (tinyperl-pod-manpage-to-file url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-perl-1 (file &optional method)
  "Go to Perl FILE and put point to optional METHOD."
  (let ((regexp (if method
		    (concat "^[ \t]*sub[ \t\n\r]*"
			    method
			    "\\>")))
	elt)
    (if (null (setq elt (tinyperl-locate-library file)))
        (message "TinyUrl: No Perl module found, %s" file)
      (switch-to-buffer (tinyperl-library-find-file elt))
      (when method
        (unless (re-search-forward regexp nil t)
          (message "TinyUrl: Hm, can't find sub using [%s]" regexp))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-perl-method (url)
  "Find Perl Foo::Bar->new(...) URL."
  (let (file
	method)
    (when (string-match "\\([^ \t\n]+\\)->\\([^ \t\n]+\\)" url)
      (setq file   (match-string 1 url)
            method (match-string 2 url)))
    (cond
     ((null file)
      (message "TinyUrl: Opps, odd perl URL %s" url)
      (sleep-for 1))
     (t
      (tinyurl-find-url-perl-1 file method)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-perl (url)
  "Find Perl `require' and `use' URL."
  (let (file)
    (cond
     ((setq file (ti::string-match
                  "use[ \t]+\\([^ \t\n;]+\\)" 1 url))
      (setq file (concat file ".pm")))
     ((setq file (ti::string-match
                  "require[ \t'\"]+\\([^ '\"\t\n;]+\\)" 1 url))))
    (cond
     ((null file)
      (message "TinyUrl: Opps, odd perl URL %s" url)
      (sleep-for 1))
     (t
      (tinyurl-find-url-perl-1 file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-perl-compile  (url &optional noerr)
  "Parse Perl compile output style URL.

    error in file FILE at line LINE
    at FILE line LINE.

If NOERR is non-nil, signal no error if file does not exist."
  (let ((fid "tinyurl-find-url-perl-compile:")
	file
	line)
    (cond
     ((or (string-match "in file +\\([^ \t\n]+\\) at line \\([0-9]+\\)" url)
          (string-match "at +\\([^ \t\n]+\\) line \\([0-9]+\\)" url))
      (setq file (match-string 1 url)
            line (string-to-number (match-string 2 url)))))
    (tinyurl-debug fid 'url url 'file file 'line line)
    (if (null file)
        (error "Tinyurl: Can't recognize URL [%s]" url))

    (cond
     ((or (ti::find-file-or-window file line 'must-exist)
          ;;  drop path name
          (ti::find-file-or-window (file-name-nondirectory file)
                                   line 'must-exist))
      t)                                ;ok
     (t
      (unless noerr
        (error "TinyUrl: Can't locate %s" file))
      nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-file-name-filter  (url &optional line)
  "Check URL and LINE for filter in `tinyurl--file-filter-table'.
Return:
  non-nil if Filter was used."
  (let ((filter (tinyurl-get-filter url)))
    (tinyurl-debug "tinyurl-file-name-filter" url filter)
    (cond
     ((stringp filter)
      (shell-command (format filter url))
      t)
     ((and (not (ti::bool-p filter))
           (fboundp filter))
      (funcall filter url)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-guess-line-number-at-point ()
  "Read current line and guess the line number."
  (let ()
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at ".*[ \t]+line[ \t]+\\([0-9]+\\)")
        (string-to-number (match-string 1)))
       ((looking-at ".*on line +<b>\\([0-9]+\\)</b>")
        ;; PHP writes HTML =>   </b> on line <b>161</b><br>
        (string-to-number (match-string 1)))
       ((looking-at "^.+:\\([0-9]+\\):")
        ;;  Grep output
        ;; test.pl:119:use integer;
        (string-to-number (match-string 1)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-file (url &optional line)
  "Go to ULR and optional LINE.
If LINE is not given, it is guessed freom the context.
Convert URL ftp:// to ange-ftp format and use `find-file'."
  (let ((fid  "tinyurl-find-url-file: ")
	(info (ti::file-path-and-line-info url)))
    (when info
      (setq url (car info)))
    (unless line
      (setq line (tinyurl-guess-line-number-at-point)))
    (setq url (cond
               ((string-match "://" url)
                (ti::string-url-to-ange-ftp url))
               ((string-match "file:\\(.*\\)" url)
                (match-string 1 url))
               (t
                url)))
    (tinyurl-debug fid 'URL url 'INFO info 'LINE line)
    (unless (integerp line) ;; Make sure it's integer
      (setq line nil))
    (cond
     ((tinyurl-file-name-filter url line))
     (t
      (ti::select-frame-non-dedicated)
      (prog1 (ti::find-file-or-window url line (not 'must-exist) info)
        (when info
          (ti::goto-line (cdr info))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-mail-browse-url (url)
  "Call brose-url with argument URL"
  (unless (string-match "^mailto:" url)
    ;;  Needed due to `browse-url-browser-function' which contains
    ;;     '(("^mailto:" . browse-url-mail) ...
    (setq url (concat "mailto:"  url))
    (browse-url url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-mail (url)
  "Ignore URL and call 'mail."
  (if (fboundp 'compose-mail)
      (call-interactively 'compose-mail) ;New Emacs
    (call-interactively 'mail-other-window)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-man (url)
  "Manpage URL handler."
  ;; Url can have leading or trailing spaces: "   crontab  (5)   "
  (let* ((program (if (string-match "[^ \t\r\n()]+" url)
                      (match-string 0 url)))
         (page    (if (string-match "(\\([^ \t\r\n()]+\\)" url)
                      (match-string 1 url)))
         (ref     (cond
                   ((and page
                         ;; skip basic references like: cut(1)
                         (> (string-to-number page) 1))
                    (format "%s(%s)" program page))
                   (program
                    program))))
    (man ref)))

;;; ----------------------------------------------------------------------
;;; FIXME: What about various mailing list archives?
;;; FIXME: Perhaps Message-id query should be delegated to proper archives
;;;
(defun tinyurl-find-url-message-id (url)
  "Get URL by Message-id."
  (unless (setq url (ti::string-match "<\\([^ \t\n>]+\\)>" 1 url))
    (error "TinyMail: invalid Message-id. Missing <>"))
  (tinyurl-debug "tinyurl-find-url-message-id" url)
  (setq url
        (concat
         "http://groups.google.com/groups?as_q=&as_umsgid="
         url
         ""))
  (tinyurl-agent-funcall 'url url))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-http (url)
  "Simple 'www.*' URL handler."
  (unless (string-match "://" url)
    (setq url (concat "http://" url)))
  (tinyurl-debug "tinyurl-find-url-http" url)
  (tinyurl-agent-funcall 'url url))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-win32-netscape (url)
  "External URL handler."
  (tinyurl-call-process-win32 "netscape" url))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-find-url-win32-iexplore (url)
  "External URL handler."
  (tinyurl-call-process-win32 "iexplore" url))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-cache-url (url)
  "Add URL to the beginning of buffer `tinyurl--url-cache-buffer'."
  (let ((buffer (get-buffer-create tinyurl--url-cache-buffer)))
    (tinyurl-debug "tinyurl-cache-url" url)
    (if (eq (current-buffer) buffer)
        (error "TinyUrl: Can't cache URL in `tinyurl--url-cache-buffer'")
      (with-current-buffer buffer
        (ti::pmin)
        (unless tinyurl-mode (tinyurl-mode-1 1))
        (if (re-search-forward (format "^%s$" (regexp-quote url)) nil t)
            (message "TinyUrl: already cached %s" url)
          (insert url "\n")
          (message "TinyUrl: cached %s" url))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-url-clean (url type)
  "Clean URL if needed."
  (if (not (tinyurl-type-external-p url type))
      url
    (if (stringp tinyurl--cleaner-regexp)
        (replace-regexp-in-string
         tinyurl--cleaner-regexp "" url)
      url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-type (url)
  "Return type of URL. Or all types if TYPES id non-nil, URL is then ignored.
Returned types (symbols) are:

  (external) url
             url-http
             url-ftp url-ange
             url-debian-bts
  (other)    mail
             file-code-lisp
             file-code-perl
             file-code-perl-pod-manpage
             file-code-perl-pod-module
             file-code-perl-method
             file-code-c
             file-packed
             file
             compiler-perl"
  (cond
   ;; .................................................... browser url ...
   ((string-match
     "Message-id:\\|References:\\|In[ \n\t]+Article[ \n\t]+"
     url) 'url-message-id)
   ((string-match  "\\(https?\\|telnet\\|wais\\|news\\|file\\):" url) 'url)
   ((string-match  "^[ \t]*www\\." url)         'url-http)
   ;;  my.site.com/dir/dir
   ((string-match  "^[^/]+\\....?/" url)        'url-http)
   ;;  Treat .html files through browser
   ((string-match  "ftp:[^ \t\n]+\\.s?html?" url) 'url-http)
   ((string-match  "ftp:" url)                    'url-ftp)
   ((string-match  "/[^@\n]+@[^@\n]+:" url)     'url-ange)
   ((string-match  "@\\|mailto:" url)           'mail)
   ((string-match
     "#[0-9]+\\>\\|\\<\\(RF.\\|IT.\\|O\\) +[0-9]+" url)
    'url-debian-bts)
   ;; ........................................................... code ...
   ((string-match  "(\\(load\\|load-library\\|require\\) " url)
    'file-code-lisp)
   ((string-match  "use \\|require " url)       'file-code-perl)
   ((string-match  "::.*->" url)                'file-code-perl-method)
   ;;  in the perlipc manpage.
   ;;  See p.264 in [perlipc]
   ((string-match "perl[^ \t\n]+[ \t\n]+manpage\\|\\[perl[^ \n\t]+\\]" url)
    'file-code-perl-pod-manpage)
   ((string-match "\\<perl[^ \t\n]+\\." url)
    'file-code-perl-pod-manpage)
   ((string-match "::.*manpage" url)            'file-code-perl-pod-module)
   ((string-match  "#include" url)              'file-code-c)
   ;; ...................................................... compilers ...
   ((string-match " parse error in <b>.*</b> on line" url)
    'compiler-php-at-line)
   ((string-match " in file.*at line " url)     'compiler-perl-in-file-at-line)
   ((string-match " at .* line " url)           'compiler-perl-at-line)
   ;; ................................................... system files ...
   ((string-match "\\.tar\\|\\.gz\\.tgz" url)   'file-packed)
   ((string-match "[/\\]" url)                  'file)
   ((string-match "^[^ \t\n]+:[0-9]+:" url)     'file) ;; file.txt:line:
   ((string-match "[a-z.]+(.*)" url)            'man)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-type-external-p (url type)
  "Check if TYPE is external. URL is unused."
  (string-match "url\\|ftp" (symbol-name type)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-handler-mail-after (url &optional type)
  "Compose URL as mail. Optional TYPE can be given."
  (let ((fid "tinyurl-handler-mail-after")
        point
        to
        subject)
    (catch 'done
      ;;  ffap would send mailto: to the ffap-url-fetcher which
      ;;  usually is 'browse-url-netscape, but you really
      ;;  don't want to compose mail with it...
      (setq url (replace-regexp-in-string "mailto:" "" url))
      ;; mailto:a@b.com?subject=test
      (setq to      (ti::string-match "[^?]+" 0 url)
            subject (ti::string-match "\\?Subject=\\([^?]+\\)" 1 url))
      ;;  This can also be a external call, like Mozilla mail...
      (tinyurl-debug fid 'url url 'to to 'subject subject)
      (tinyurl-agent-funcall 'mail url)
      (setq point (point))
      (ti::pmin)
      (unless (re-search-forward "^To: " nil t)
        (goto-char point)
        (throw 'done t))
      (insert to)
      (ti::pmin)
      (unless (re-search-forward "^Subject: "  nil t)
        (goto-char point)
        (message "TinyUrl: [ERROR] Cannot continue,Subject: not found")
        (throw 'done t))
      (when subject
        (insert (replace-regexp-in-string "[%]20" " " subject))
        (ti::mail-text-start 'move))
      ;;   We can be a bit smarter, Usually the mailing linst have
      ;;   address xxx-request@foo.com, so add implicit "subsribe"
      ;;   to the subject fields. User may add "un" if he wants that
      ;;   instead.
      (save-excursion
        (cond
         ((string-match "-request@" to)
          (insert "subscribe")))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-handler-main (url &optional type)
  "Handle URL and forward it to right agent function. TYPE of url can be given.
References: `tinyurl--command-table'"
  (let ((fid       "tinyurl-handler-main")
	(raw-list '(url-message-id
		    compiler-perl-in-file-at-line
		    compiler-perl-at-line))
	(unplugged (not (funcall tinyurl--plugged-function)))
	func
	to
	subject
	clean)
    (or type
        (setq type (tinyurl-type url)))
    (setq clean  (tinyurl-url-clean url type))
    (tinyurl-debug fid 'TYPE type 'URL url 'CLEAN clean 'PLUGGED unplugged)
    (message "TinyUrl: Accessing %s" clean)
    (cond
     ((eq type nil)
      (message "TinyUrl: Strange Error, Couldn't detect URL type: [%s] [%s]"
               url clean))
     ((eq type 'mail)
      (tinyurl-handler-mail-after clean type))
     (t
      (if (and (tinyurl-type-external-p clean type)
               unplugged)
          (tinyurl-cache-url url)
        (setq func (tinyurl-agent-function type))
        (tinyurl-debug fid 'LAST-CASE-TYPE type 'FUNC func 'URL url clean)
        (tinyurl-debug fid 'FUNCALL func 'URL url 'CLEAN clean)
        (if (memq type raw-list)
            (funcall func url)          ;RAW
          (funcall func clean)))))))

;;}}}
;;{{{ Marking line

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mark-process-post-command  ()
  "Used in `post-command-hook'."
  (when tinyurl-mode
    (let (counter)
      (unless (integerp (setq counter (get 'tinyurl-mode 'counter)))
        (setq counter 0))
      (incf  counter)
      (put 'tinyurl-mode 'counter counter)
      ;;  Activate only every 5th time.
      (when (zerop (% counter tinyurl--post-command-hook-threshold))
        (put 'tinyurl-mode 'counter 0)
        (tinyurl-mark-process)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mark-mouse ()
  "Mark URLs on current mouse line."
  (when (fboundp 'mouse-position)
    (multiple-value-bind (line col)
        (ti::compat-mouse-position-coordinates)
      (when (and line col)
        (save-excursion
          (goto-char (window-start))
          (forward-line line)
;;;          (ti::d! (ti::read-current-line))
          (let ((end (line-end-position)))
            (when (not (eq end
                           (get 'tinyurl-mark-line 'mouse)))
              (put 'tinyurl-mark-line 'mouse end)
              (tinyurl-mark-line))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mark-process ()
  "Mark URLs on current line and `mouse-position'."
  (when (and (or tinyurl-mode
                 (and (null tinyurl--mode-manually-turned-off)
                      (get  'tinyurl-mode 'global)
                      ;; Auto-activate if URL appear anywhere in buffer
                      (and (fboundp tinyurl--auto-activate-function)
                           (funcall tinyurl--auto-activate-function)))))
    ;; Check if we have already marked this line
    (let ((end (line-end-position)))
      (when (not (eq end
                     (get 'tinyurl-mark-line 'point)))
        (put 'tinyurl-mark-line 'point end)
        (tinyurl-mark-line)))
    (tinyurl-mark-mouse)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-default-mark-table ()
  "Return default table used by `tinyurl-mark-line'.
Format:
  '( (REGEXP [SUB-MATCH] [SPAN-FLAG] [VALIDATE-HANDLER]) ..)

REGEXP      To mark the URL
SUB-MATCH   In REGEXP to match URL
SPAN-FLAG   If non-nil, then regexp match does not end to the end of
            current line.
VALIDATE-HANDLER    Function to discard and check marked url"
  (let* ((site       "[-a-z0-9.]+")
         (white      " \t\r\n\f")       ;whitespace
         (white-file " *?\t\r\n\f")     ;whitespace, exclude wildcards
         (white-re   `,(concat "[" white "]"))
         (nwhite-re  `,(concat "[^" white "]"))
         (word       `,(concat "[^][(){}<>$^*?:\"'" white "]")) ;; filename word
         (word+      `,(concat word "+"))
;;;      (word*      `,(concat word "*"))
         (url-word+  `,(concat "[^][{}<>$^*\"'" white "]+")) ;; include ?
         (url-word*  `,(concat "[^][{}<>$^*\"'" white "]*"))
         (non-spc    `,(concat "[^\"';" white "]"))
         (non-spc+   `,(concat non-spc "+"))
         ;;      (non-spc*  (concat non-spc "*"))
         (slash    (if (ti::win32-p)
                       "\\/"            ;Accept both
                     "/"))              ;only in Unix
         (slash-re (format "[%s]" slash))
         (drive    (if (ti::win32-p)
                       "[a-zA-Z]:"      ; D:\dir\file.txt
                     ""))               ; In Unix no drive letter
         (compiler-number "\\(:[0-9]+:\\)")
         (maybe-number "\\(:[0-9]+\\)?")
         (table
          (list
           (list `,(concat "\\<mailto:" white-re "*" nwhite-re "+") 0 'span)
           ;; This must come first
           (list "<URL:\\([^>]+\\)>" 1 'span)
           (list
            `,(concat
	       "\\(Message-Id:\\|References:\\|In Article\\)"
	       white-re "*<[^>" white  "]+>")
            0
            'span)
           (list
            `,(concat
	       "\\(\\(\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\|news\\)://"
	       "\\|file:/\\)\\)"
	       url-word+)
            0)
           (list tinyurl--email-regexp 0 nil 'tinyurl-validate-url-email)
           ;; If it starets with "www" and ends to 2-3 characters, it must
           ;; be http pointer
           ;;
           ;; The regexp starts with "[^/], so that http://www match isn't
           ;; replaced with this.
           (list
            `,(concat
	       "[^/]www\\.\\([-a-z0-9]+\.\\)+[a-z][a-z][a-z]?"
	       url-word*
	       "\\>")
            0)
           ;; C/C++
           ;;
           ;;   #include <stdio.h>
           (list
            `,(concat
	       "^[ \t]*#include +<[^>]+>"
	       "\\|^[ \t]*#include +\"[^\"]+\"")
            0)
           ;; Debian
           ;;
           ;;   bug#NNNNNN
           ;;   RFA NNNNNN package -- ...
           ;;   O NNNNNN package -- ...
           ;;   Closes: #NNNNNN
           ;;
           (list
            `,(concat
	       "\\<bug#[0-9]+\\>"
	       "\\|Closes:? +#[0-9]+\\>"
	       "\\|\\<\\(RF.\\|IT\\.\\|O\\) +[0-9]+\\>")
            0)
           ;; Perl code statements
           ;;
           ;;   require 'library.pl';
           ;;   use      Module;
           (list
            `,(concat
	       "\\<require[ \t\"']+[_a-z0-9.]+pl[ \t\"']*;"
	       "\\|\\<use[ \t]+[_a-z0-9:]+[ \t]*;")
            0)
           ;;  Perl Foo::Bar->new(...);
           '("\\<[A-Za-z]+::[A-Za-z]+\\(->[A-Za-z]+\\)?"
             0
             nil
             tinyurl-validate-url-perl-method)
           ;; Browsing Perl POD pages
           ;;
           ;;   "See perlfunc manpage"
           ;;   "See [perlfaq2]"
           ;;   Devel::DProf manpage
           ;;
           ;; SEE ALSO
           ;;    perlrequick.
           ;;    perlretut.
           ;;    "Regexp Quote-Like Operators" in perlop.
           (list
            `,(concat
	       "\\<perl" nwhite-re "+" white-re "+manpage"
	       "\\|\\[perl" nwhite-re "+\\]"
	       "\\|[A-Z][a-z]+::[A-Z][a-z]" white-re "+manpage"
	       "\\|^[ \t]+perl" nwhite-re "+\\.[ \t]*$"
	       "\\|in[ \t]+perl" nwhite-re "+\\.[ \t]*$")
            0
            'span)
           ;; [Compiler output]
           ;; Perl error messages
           ;;
           ;;       Global symbol "x" requires explicit package name
           ;;       at /users/foo/bin/file.pl line 289.
           ;;
           ;;       syntax error in file ./fle.pl at line 268
           (list
            `,(concat
	       " in file +" non-spc+ " +at +line +[0-9]+"
	       "\\| at +"   non-spc+ " +line +[0-9]+")
            0)
           ;; Manual pages . Examples from HP-UX
           ;;
           ;;   cut(1), ypmake(1M), unistd.h(5), typeahead(3X)
           ;;   termios(7), sshd(8), html2ps(1) ssh-agent(1)
           ;;   crontab(5)
           (list
            "\\<[-_a-z.0-9]+([1-9][CMSX]?)"
            0)
           ;; Lisp
           ;;
           ;;  (load            "file.el")
           ;;  (load-library    "file.el")
           ;;  (load-file       "file.el")
           ;;  (require 'feature)
           (list
            `,(concat
	       "(\\(load\\|load-library\\|load-file\\|require\\)[ \t\"']+"
	       word+)
            0)
           ;; ............................................ local files ...
           ;; Local files, this must be last because the regexp is "loose"
           ;; and would match if put above.
           ;;
           ;;   ~foo/dir/file.txt
           ;;   /users/foo/file.txt
           ;;   /usr/include/shadow.h:8
           ;;
           ;;   D:\dir\dir\file.txt
           ;;   D:/dir/dir/file.txt
           ;;   //server/dir/that/there
           ;;
           ;; This still highlights statement like /.*
           ;; Can't do nothing about that. I don't want to make enourmously
           ;; complex regexp NOT to match false filenames. So we have to
           ;; bear with some mishits
           ;;
           ;; () grouped regexp reads:  (SLASH NOT-SLASH|~SLASH)word*nbr?
           ;; The purpose is not to match double slash C++ comments //
           (list
            `,(concat
	       ;; Must be at the begining of line, or after whitespace
	       "\\(^\\|[ \t]\\)"
	       "\\(" "\\(" drive "\\|//\\|[\\][\\]\\)?"
	       slash-re "[^" white-file slash "]"
	       "\\|~" slash-re "?\\)"
	       word+
	       maybe-number
	       ;;  Must be 'alone' and separated from others.
	       "\\([ \t]\\|$\\)")
            0
            nil
            'tinyurl-validate-url-default)
           ;; Last try, the file may be inside Emacs already
           ;;  this-file.el:12:   The matched line...
           (list
            `,(concat "^\\(" nwhite-re "+\\)" compiler-number )
            0
            nil
            'tinyurl-validate-url-default)))) ;; list of regexps end
    table))

;;; ----------------------------------------------------------------------
;;;
(defun tinyurl-mark-line ()
  "Mark URLs with overlays on current line.
Return:
   list of overlays where the regexps matched.

  '((ov ov ..) (regexp regexp ..))"
  (interactive)
  (let ((fid      "tinyurl-mark-line:")
	(plist    (tinyurl-overlay-plist))
	(table    (tinyurl-default-mark-table))
	regexp
	level
	function
	url
	ov-stat
	olist
	ov-list
	match-list
	end
	type)
    ;;  Delete old overlays first
    (tinyurl-overlay-kill)
    (tinyurl-modeline-update) ;; update plugged status
    ;;   Now mark all urls with overlays on current line
    ;;   OV-LIST contains generated overlays.
    (save-excursion
      ;; Allow line span (setq end (line-end-position))
      (beginning-of-line)
      (dolist (elt table)
        (setq regexp   (nth 0 elt)
              level    (or (nth 1 elt) 0)
              end      (line-end-position)
              type     (nth 2 elt)
              function (nth 3 elt)
              olist    nil)
        ;;  If it is allowed to span multiple lines,
        ;;  then limit the scanning to average of 3 lines
        ;;  whose length is estimated 50 characters.
        ;;
        ;;  Adjust calculated pos according to point-max
        (cond
         ((eq type 'no-limit)
          (setq end nil))
         ((and (eq type 'span)
               ;; There must be spanning url in this line
               (string-match regexp
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (min
                               (+ 200 (line-beginning-position))
                               (point-max)))))
          (setq end
                (let ((pos (+ (point) (* 3 50))))
                  (if (> pos (point-max))
                      (point-max)
                    pos)))))
        ;; ................................................... do work ...
        (tinyurl-debug fid "DOLIST-ELT: " end elt)
        (if (and plist
                 (stringp regexp))
            (setq olist
                  (nth 1
                       (ti::overlay-re-search
                        regexp
                        level
                        plist
                        end             ;MAX-POINT
                        nil nil nil     ;BACK REUSE REUSE-P
                        '(owner tinyurl)))))
        (when olist
          (tinyurl-debug fid "OVERLAY-LIST" olist)
          (dolist (ov olist)
            (cond
             ((not (overlayp ov))
              (message "TinyUrl: ERROR, non-overlay %s"
                       (prin1-to-string ov))
              (tinyurl-debug fid 'NON-OVERLAY ov))
             (t
              (setq url (ti::overlay-buffer-substring ov 'no-properties))
              ;;  - If some previous regexp marks identical overlay,
              ;;    do not add it to the list.
              (when (and ov-list
                         (not (member url ov-list)))
                (setq ov-stat (or (null function)
                                  (funcall function url)))
                (tinyurl-debug fid 'STATUS ov-stat 'FUNC function 'URL url "\n")
                (cond
                 ((eq ov-stat t)
                  (push regexp match-list)
                  (push ov     ov-list))
                 (t
                  (delete-overlay ov))))))))))
    ;;  Save the created overlay list, we don't want to bloat buffer
    ;;  full of overlays.
    (put 'tinyurl-mark-line 'ov-list    ov-list)
    (put 'tinyurl-mark-line 'match-list match-list)
    (tinyurl-debug fid "RET OV-LIST" ov-list)
    (when (and ov-list match-list)      ;Return value
      (list ov-list match-list))))

;;}}}

(add-hook 'tinyurl--mode-define-keys-hook 'tinyurl-mode-define-keys)

(tinyurl-install)

(provide   'tinyurl)
(run-hooks 'tinyurl--load-hook)

;;; tinyurl.el ends here
