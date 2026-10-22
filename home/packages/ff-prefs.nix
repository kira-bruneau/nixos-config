{ writers }:

writers.writeNuBin "ff-prefs" ''
  export def main [
    file?: string
    --no-parse (-n)
    --json (-j)
  ] {
    if $file != null { open --raw $file } else { decode } |
    parse --regex 'user_pref\((?<key>".*"), (?<value>.*)\)' |
    each { {
      key: ($in.key | from json)
      value: ($in.value | from json)
    } } |
    if $no_parse { $in } else { parse-ff-prefs } |
    transpose -rd |
    if $json { to json } else { $in }
  }

  def parse-ff-prefs [] {
    each { merge {
      value: (match $in.key {
        'services.settings.base_attachments_url' => (
          $in.value | parse '{server_url}|{base_url}'
        )
        'devtools.toolbox.tabsOrder' |
        'privacy.fingerprintingProtection.overrides' |
        'services.sync.declinedEngines' => (
          $in.value | split row ','
        )
        'app.update.lastUpdateTime.addon-background-update-timer' |
        'app.update.lastUpdateTime.browser-cleanup-thumbnails' |
        'app.update.lastUpdateTime.services-settings-poll-changes' |
        'app.update.lastUpdateTime.xpi-signature-verification' |
        'browser.shell.mostRecentDateSetAsDefault' |
        'browser.startup.lastColdStartupCheck' |
        'browser.urlbar.lastUrlbarSearchSeconds' |
        'extensions.getAddons.cache.lastUpdate' |
        'identity.fxaccounts.commands.missed.last_fetch' |
        'idle.lastDailyNotification' |
        'media.gmp-gmpopenh264.lastDownload' |
        'media.gmp-gmpopenh264.lastInstallStart' |
        'media.gmp-gmpopenh264.lastUpdate' |
        'media.gmp-manager.lastCheck' |
        'media.gmp-manager.lastEmptyCheck' |
        'media.videocontrols.picture-in-picture.video-toggle.first-seen-secs' |
        'places.database.lastMaintenance' |
        'privacy.purge_trackers.last_purge' |
        'services.settings.blocklists.addons-bloomfilters.last_check' |
        'services.settings.blocklists.gfx.last_check' |
        'services.settings.last_etag' |
        'services.settings.last_update_seconds' |
        'services.settings.main.anti-tracking-url-decoration.last_check' |
        'services.settings.main.bounce-tracking-protection-exceptions.last_check' |
        'services.settings.main.cookie-banner-rules-list.last_check' |
        'services.settings.main.fingerprinting-protection-overrides.last_check' |
        'services.settings.main.fxmonitor-breaches.last_check' |
        'services.settings.main.hijack-blocklists.last_check' |
        'services.settings.main.language-dictionaries.last_check' |
        'services.settings.main.partitioning-exempt-urls.last_check' |
        'services.settings.main.password-recipes.last_check' |
        'services.settings.main.password-rules.last_check' |
        'services.settings.main.query-stripping.last_check' |
        'services.settings.main.third-party-cookie-blocking-exempt-urls.last_check' |
        'services.settings.main.tracking-protection-lists.last_check' |
        'services.settings.main.translations-models-v2.last_check' |
        'services.settings.main.translations-models.last_check' |
        'services.settings.main.translations-wasm.last_check' |
        'services.settings.main.url-classifier-exceptions.last_check' |
        'services.settings.main.webcompat-interventions.last_check' |
        'services.settings.main.websites-with-shared-credential-backends.last_check' |
        'services.settings.security-state.cert-revocations.last_check' |
        'services.settings.security-state.intermediates.last_check' |
        'services.settings.security-state.onecrl.last_check' |
        'services.sync.clients.lastRecordUpload' |
        'services.sync.lastPing' |
        'services.sync.lastTabFetch' |
        'services.sync.nextSync' |
        'storage.vacuum.last.content-prefs.sqlite' |
        'storage.vacuum.last.places.sqlite' |
        'toolkit.startup.last_success' => (
          $in.value | into datetime -f '%s'
        )
        'distribution.iniFile.exists.appversion' |
        'extensions.lastAppVersion' |
        'extensions.lastPlatformVersion' |
        'services.sync.clients.lastSync' |
        'services.sync.forms.lastSync' |
        'services.sync.tabs.lastSync' => (
          $in.value | into float | $in * 1e9 | into int | into datetime
        )
        'services.sync.lastSync' => (
          $in.value | parse --regex '(?<datetime>.*).*\(.*\)' | get datetime | into datetime
        )
        _ => (let value = $in.value; (
          try { $value | from json }
          catch { $value }
        ))
      })
    } }
  }
''
