



<!DOCTYPE html>
<html lang="en" class="">
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# object: http://ogp.me/ns/object# article: http://ogp.me/ns/article# profile: http://ogp.me/ns/profile#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta http-equiv="Content-Language" content="en">
    
    
    <title>RepData_PeerAssessment_master/PA1_template.md at master · winnersky/RepData_PeerAssessment_master</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-114.png">
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114.png">
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-144.png">
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144.png">
    <meta property="fb:app_id" content="1401488693436528">

      <meta content="@github" name="twitter:site" /><meta content="summary" name="twitter:card" /><meta content="winnersky/RepData_PeerAssessment_master" name="twitter:title" /><meta content="RepData_PeerAssessment_master - RepData_PeerAssessment1" name="twitter:description" /><meta content="https://avatars0.githubusercontent.com/u/4812870?v=3&amp;s=400" name="twitter:image:src" />
<meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="https://avatars0.githubusercontent.com/u/4812870?v=3&amp;s=400" property="og:image" /><meta content="winnersky/RepData_PeerAssessment_master" property="og:title" /><meta content="https://github.com/winnersky/RepData_PeerAssessment_master" property="og:url" /><meta content="RepData_PeerAssessment_master - RepData_PeerAssessment1" property="og:description" />

      <meta name="browser-stats-url" content="/_stats">
    <link rel="assets" href="https://assets-cdn.github.com/">
    <link rel="conduit-xhr" href="https://ghconduit.com:25035">
    <link rel="xhr-socket" href="/_sockets">
    <meta name="pjax-timeout" content="1000">
    <link rel="sudo-modal" href="/sessions/sudo_modal">

    <meta name="msapplication-TileImage" content="/windows-tile.png">
    <meta name="msapplication-TileColor" content="#ffffff">
    <meta name="selected-link" value="repo_source" data-pjax-transient>
      <meta name="google-analytics" content="UA-3769691-2">

    <meta content="collector.githubapp.com" name="octolytics-host" /><meta content="collector-cdn.github.com" name="octolytics-script-host" /><meta content="github" name="octolytics-app-id" /><meta content="77213A57:6263:F46EBA:548DEFBC" name="octolytics-dimension-request_id" /><meta content="8355200" name="octolytics-actor-id" /><meta content="zhoukelvin" name="octolytics-actor-login" /><meta content="5034f8cb4c930a885b8bb0c446b51a8ae49b0c5b13c39fa68c3bb17426079660" name="octolytics-actor-hash" />
    
    <meta content="Rails, view, blob#show" name="analytics-event" />

    
    
    <link rel="icon" type="image/x-icon" href="https://assets-cdn.github.com/favicon.ico">


    <meta content="authenticity_token" name="csrf-param" />
<meta content="aOUv48jSqf89FOgQjFpDMuGe/k+hBXBcalNgBgXtxLPpxgFcdORe85wnjVPTQZB6eMG9W3RpadoeArB6bjfIGA==" name="csrf-token" />

    <link href="https://assets-cdn.github.com/assets/github-c7f434a471766a748d862b4cb294526acd6901321d901cc64028f53af8490603.css" media="all" rel="stylesheet" type="text/css" />
    <link href="https://assets-cdn.github.com/assets/github2-761398b51b4a87682bde8f8b3479da06f8bde788a3fae6fb10b018a384748cf0.css" media="all" rel="stylesheet" type="text/css" />
    
    


    <meta http-equiv="x-pjax-version" content="52744ce9703c979de4f8f2f444b85ff4">

      
  <meta name="description" content="RepData_PeerAssessment_master - RepData_PeerAssessment1">
  <meta name="go-import" content="github.com/winnersky/RepData_PeerAssessment_master git https://github.com/winnersky/RepData_PeerAssessment_master.git">

  <meta content="4812870" name="octolytics-dimension-user_id" /><meta content="winnersky" name="octolytics-dimension-user_login" /><meta content="20760980" name="octolytics-dimension-repository_id" /><meta content="winnersky/RepData_PeerAssessment_master" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="20760980" name="octolytics-dimension-repository_network_root_id" /><meta content="winnersky/RepData_PeerAssessment_master" name="octolytics-dimension-repository_network_root_nwo" />
  <link href="https://github.com/winnersky/RepData_PeerAssessment_master/commits/master.atom" rel="alternate" title="Recent Commits to RepData_PeerAssessment_master:master" type="application/atom+xml">

  </head>


  <body class="logged_in  env-production windows vis-public page-blob">
    <a href="#start-of-content" tabindex="1" class="accessibility-aid js-skip-to-content">Skip to content</a>
    <div class="wrapper">
      
      
      
      


      <div class="header header-logged-in true" role="banner">
  <div class="container clearfix">

    <a class="header-logo-invertocat" href="https://github.com/" data-hotkey="g d" aria-label="Homepage" ga-data-click="Header, go to dashboard, icon:logo">
  <span class="mega-octicon octicon-mark-github"></span>
</a>


      <div class="site-search repo-scope js-site-search" role="search">
          <form accept-charset="UTF-8" action="/winnersky/RepData_PeerAssessment_master/search" class="js-site-search-form" data-global-search-url="/search" data-repo-search-url="/winnersky/RepData_PeerAssessment_master/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
  <input type="text"
    class="js-site-search-field is-clearable"
    data-hotkey="s"
    name="q"
    placeholder="Search"
    data-global-scope-placeholder="Search GitHub"
    data-repo-scope-placeholder="Search"
    tabindex="1"
    autocapitalize="off">
  <div class="scope-badge">This repository</div>
</form>
      </div>
      <ul class="header-nav left" role="navigation">
        <li class="header-nav-item explore">
          <a class="header-nav-link" href="/explore" data-ga-click="Header, go to explore, text:explore">Explore</a>
        </li>
          <li class="header-nav-item">
            <a class="header-nav-link" href="https://gist.github.com" data-ga-click="Header, go to gist, text:gist">Gist</a>
          </li>
          <li class="header-nav-item">
            <a class="header-nav-link" href="/blog" data-ga-click="Header, go to blog, text:blog">Blog</a>
          </li>
        <li class="header-nav-item">
          <a class="header-nav-link" href="https://help.github.com" data-ga-click="Header, go to help, text:help">Help</a>
        </li>
      </ul>

    
<ul class="header-nav user-nav right" id="user-links">
  <li class="header-nav-item dropdown js-menu-container">
    <a class="header-nav-link name" href="/zhoukelvin" data-ga-click="Header, go to profile, text:username">
      <img alt="zhoukelvin" class="avatar" data-user="8355200" height="20" src="https://avatars0.githubusercontent.com/u/8355200?v=3&amp;s=40" width="20" />
      <span class="css-truncate">
        <span class="css-truncate-target">zhoukelvin</span>
      </span>
    </a>
  </li>

  <li class="header-nav-item dropdown js-menu-container">
    <a class="header-nav-link js-menu-target tooltipped tooltipped-s" href="#" aria-label="Create new..." data-ga-click="Header, create new, icon:add">
      <span class="octicon octicon-plus"></span>
      <span class="dropdown-caret"></span>
    </a>

    <div class="dropdown-menu-content js-menu-content">
      
<ul class="dropdown-menu">
  <li>
    <a href="/new"><span class="octicon octicon-repo"></span> New repository</a>
  </li>
  <li>
    <a href="/organizations/new"><span class="octicon octicon-organization"></span> New organization</a>
  </li>


    <li class="dropdown-divider"></li>
    <li class="dropdown-header">
      <span title="winnersky/RepData_PeerAssessment_master">This repository</span>
    </li>
      <li>
        <a href="/winnersky/RepData_PeerAssessment_master/issues/new"><span class="octicon octicon-issue-opened"></span> New issue</a>
      </li>
</ul>

    </div>
  </li>

  <li class="header-nav-item">
        <a href="/notifications" aria-label="You have no unread notifications" class="header-nav-link notification-indicator tooltipped tooltipped-s" data-ga-click="Header, go to notifications, icon:read" data-hotkey="g n">
        <span class="mail-status all-read"></span>
        <span class="octicon octicon-inbox"></span>
</a>
  </li>

  <li class="header-nav-item">
    <a class="header-nav-link tooltipped tooltipped-s" href="/settings/profile" id="account_settings" aria-label="Settings" data-ga-click="Header, go to settings, icon:settings">
      <span class="octicon octicon-gear"></span>
    </a>
  </li>

  <li class="header-nav-item">
    <form accept-charset="UTF-8" action="/logout" class="logout-form" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="cA9w4b73ynxJPcOzQDs5aMAi3aGdaYvJLZLlnE22KVboNzKnSxQa/FvglSBfGMsZHckLm1dGAx1tiV9XnWyaEw==" /></div>
      <button class="header-nav-link sign-out-button tooltipped tooltipped-s" aria-label="Sign out" data-ga-click="Header, sign out, icon:logout">
        <span class="octicon octicon-sign-out"></span>
      </button>
</form>  </li>

</ul>


    
  </div>
</div>

      

        


      <div id="start-of-content" class="accessibility-aid"></div>
          <div class="site" itemscope itemtype="http://schema.org/WebPage">
    <div id="js-flash-container">
      
    </div>
    <div class="pagehead repohead instapaper_ignore readability-menu">
      <div class="container">
        
<ul class="pagehead-actions">

    <li class="subscription">
      <form accept-charset="UTF-8" action="/notifications/subscribe" class="js-social-container" data-autosubmit="true" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="C5lJIa0ujL7manoNZZj6xpiIRvp4QwVohRW5ix4693F9vl49eHZMy4e0HfzOyEzqTHLEwY2CoUYSvZ2m8LfT2g==" /></div>  <input id="repository_id" name="repository_id" type="hidden" value="20760980" />

    <div class="select-menu js-menu-container js-select-menu">
      <a class="social-count js-social-count" href="/winnersky/RepData_PeerAssessment_master/watchers">
        1
      </a>
      <a href="/winnersky/RepData_PeerAssessment_master/subscription"
        class="minibutton select-menu-button with-count js-menu-target" role="button" tabindex="0" aria-haspopup="true">
        <span class="js-select-button">
          <span class="octicon octicon-eye"></span>
          Watch
        </span>
      </a>

      <div class="select-menu-modal-holder">
        <div class="select-menu-modal subscription-menu-modal js-menu-content" aria-hidden="true">
          <div class="select-menu-header">
            <span class="select-menu-title">Notifications</span>
            <span class="octicon octicon-x js-menu-close" role="button" aria-label="Close"></span>
          </div> <!-- /.select-menu-header -->

          <div class="select-menu-list js-navigation-container" role="menu">

            <div class="select-menu-item js-navigation-item selected" role="menuitem" tabindex="0">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <div class="select-menu-item-text">
                <input checked="checked" id="do_included" name="do" type="radio" value="included" />
                <h4>Not watching</h4>
                <span class="description">Be notified when participating or @mentioned.</span>
                <span class="js-select-button-text hidden-select-button-text">
                  <span class="octicon octicon-eye"></span>
                  Watch
                </span>
              </div>
            </div> <!-- /.select-menu-item -->

            <div class="select-menu-item js-navigation-item " role="menuitem" tabindex="0">
              <span class="select-menu-item-icon octicon octicon octicon-check"></span>
              <div class="select-menu-item-text">
                <input id="do_subscribed" name="do" type="radio" value="subscribed" />
                <h4>Watching</h4>
                <span class="description">Be notified of all conversations.</span>
                <span class="js-select-button-text hidden-select-button-text">
                  <span class="octicon octicon-eye"></span>
                  Unwatch
                </span>
              </div>
            </div> <!-- /.select-menu-item -->

            <div class="select-menu-item js-navigation-item " role="menuitem" tabindex="0">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <div class="select-menu-item-text">
                <input id="do_ignore" name="do" type="radio" value="ignore" />
                <h4>Ignoring</h4>
                <span class="description">Never be notified.</span>
                <span class="js-select-button-text hidden-select-button-text">
                  <span class="octicon octicon-mute"></span>
                  Stop ignoring
                </span>
              </div>
            </div> <!-- /.select-menu-item -->

          </div> <!-- /.select-menu-list -->

        </div> <!-- /.select-menu-modal -->
      </div> <!-- /.select-menu-modal-holder -->
    </div> <!-- /.select-menu -->

</form>
    </li>

  <li>
    
  <div class="js-toggler-container js-social-container starring-container ">

    <form accept-charset="UTF-8" action="/winnersky/RepData_PeerAssessment_master/unstar" class="js-toggler-form starred js-unstar-button" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="iYeD8ke1SOO5ZDS0f2gewKtQZwEVEuXK+sfu/TyEijFj8IjPY7AkxqPa5h1E665QA/JgGo6YNN54a7XfrL5M7A==" /></div>
      <button
        class="minibutton with-count js-toggler-target star-button"
        aria-label="Unstar this repository" title="Unstar winnersky/RepData_PeerAssessment_master">
        <span class="octicon octicon-star"></span>
        Unstar
      </button>
        <a class="social-count js-social-count" href="/winnersky/RepData_PeerAssessment_master/stargazers">
          0
        </a>
</form>
    <form accept-charset="UTF-8" action="/winnersky/RepData_PeerAssessment_master/star" class="js-toggler-form unstarred js-star-button" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /><input name="authenticity_token" type="hidden" value="kA83DAo2W6jOdYFf/JdK6t40j+ZOZIGHxERstpWzgkqoFhpgUw1KJjAoh59S1Hbpg9LyyFJbs0D0cNNVEIHGmg==" /></div>
      <button
        class="minibutton with-count js-toggler-target star-button"
        aria-label="Star this repository" title="Star winnersky/RepData_PeerAssessment_master">
        <span class="octicon octicon-star"></span>
        Star
      </button>
        <a class="social-count js-social-count" href="/winnersky/RepData_PeerAssessment_master/stargazers">
          0
        </a>
</form>  </div>

  </li>


        <li>
          <a href="/winnersky/RepData_PeerAssessment_master/fork" class="minibutton with-count js-toggler-target fork-button tooltipped-n" title="Fork your own copy of winnersky/RepData_PeerAssessment_master to your account" aria-label="Fork your own copy of winnersky/RepData_PeerAssessment_master to your account" rel="nofollow" data-method="post">
            <span class="octicon octicon-repo-forked"></span>
            Fork
          </a>
          <a href="/winnersky/RepData_PeerAssessment_master/network" class="social-count">4</a>
        </li>

</ul>

        <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public">
          <span class="mega-octicon octicon-repo"></span>
          <span class="author"><a href="/winnersky" class="url fn" itemprop="url" rel="author"><span itemprop="title">winnersky</span></a></span><!--
       --><span class="path-divider">/</span><!--
       --><strong><a href="/winnersky/RepData_PeerAssessment_master" class="js-current-repository" data-pjax="#js-repo-pjax-container">RepData_PeerAssessment_master</a></strong>

          <span class="page-context-loader">
            <img alt="" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
          </span>

        </h1>
      </div><!-- /.container -->
    </div><!-- /.repohead -->

    <div class="container">
      <div class="repository-with-sidebar repo-container new-discussion-timeline  ">
        <div class="repository-sidebar clearfix">
            
<nav class="sunken-menu repo-nav js-repo-nav js-sidenav-container-pjax js-octicon-loaders"
     role="navigation"
     data-pjax="#js-repo-pjax-container"
     data-issue-count-url="/winnersky/RepData_PeerAssessment_master/issues/counts">
  <ul class="sunken-menu-group">
    <li class="tooltipped tooltipped-w" aria-label="Code">
      <a href="/winnersky/RepData_PeerAssessment_master" aria-label="Code" class="selected js-selected-navigation-item sunken-menu-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches /winnersky/RepData_PeerAssessment_master">
        <span class="octicon octicon-code"></span> <span class="full-word">Code</span>
        <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>

      <li class="tooltipped tooltipped-w" aria-label="Issues">
        <a href="/winnersky/RepData_PeerAssessment_master/issues" aria-label="Issues" class="js-selected-navigation-item sunken-menu-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /winnersky/RepData_PeerAssessment_master/issues">
          <span class="octicon octicon-issue-opened"></span> <span class="full-word">Issues</span>
          <span class="js-issue-replace-counter"></span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

    <li class="tooltipped tooltipped-w" aria-label="Pull Requests">
      <a href="/winnersky/RepData_PeerAssessment_master/pulls" aria-label="Pull Requests" class="js-selected-navigation-item sunken-menu-item" data-hotkey="g p" data-selected-links="repo_pulls /winnersky/RepData_PeerAssessment_master/pulls">
          <span class="octicon octicon-git-pull-request"></span> <span class="full-word">Pull Requests</span>
          <span class="js-pull-replace-counter"></span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>


      <li class="tooltipped tooltipped-w" aria-label="Wiki">
        <a href="/winnersky/RepData_PeerAssessment_master/wiki" aria-label="Wiki" class="js-selected-navigation-item sunken-menu-item" data-hotkey="g w" data-selected-links="repo_wiki /winnersky/RepData_PeerAssessment_master/wiki">
          <span class="octicon octicon-book"></span> <span class="full-word">Wiki</span>
          <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>
  </ul>
  <div class="sunken-menu-separator"></div>
  <ul class="sunken-menu-group">

    <li class="tooltipped tooltipped-w" aria-label="Pulse">
      <a href="/winnersky/RepData_PeerAssessment_master/pulse" aria-label="Pulse" class="js-selected-navigation-item sunken-menu-item" data-selected-links="pulse /winnersky/RepData_PeerAssessment_master/pulse">
        <span class="octicon octicon-pulse"></span> <span class="full-word">Pulse</span>
        <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>

    <li class="tooltipped tooltipped-w" aria-label="Graphs">
      <a href="/winnersky/RepData_PeerAssessment_master/graphs" aria-label="Graphs" class="js-selected-navigation-item sunken-menu-item" data-selected-links="repo_graphs repo_contributors /winnersky/RepData_PeerAssessment_master/graphs">
        <span class="octicon octicon-graph"></span> <span class="full-word">Graphs</span>
        <img alt="" class="mini-loader" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
</a>    </li>
  </ul>


</nav>

              <div class="only-with-full-nav">
                
  
<div class="clone-url open"
  data-protocol-type="http"
  data-url="/users/set_protocol?protocol_selector=http&amp;protocol_type=clone">
  <h3><span class="text-emphasized">HTTPS</span> clone URL</h3>
  <div class="input-group js-zeroclipboard-container">
    <input type="text" class="input-mini input-monospace js-url-field js-zeroclipboard-target"
           value="https://github.com/winnersky/RepData_PeerAssessment_master.git" readonly="readonly">
    <span class="input-group-button">
      <button aria-label="Copy to clipboard" class="js-zeroclipboard minibutton zeroclipboard-button" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </span>
  </div>
</div>

  
<div class="clone-url "
  data-protocol-type="ssh"
  data-url="/users/set_protocol?protocol_selector=ssh&amp;protocol_type=clone">
  <h3><span class="text-emphasized">SSH</span> clone URL</h3>
  <div class="input-group js-zeroclipboard-container">
    <input type="text" class="input-mini input-monospace js-url-field js-zeroclipboard-target"
           value="git@github.com:winnersky/RepData_PeerAssessment_master.git" readonly="readonly">
    <span class="input-group-button">
      <button aria-label="Copy to clipboard" class="js-zeroclipboard minibutton zeroclipboard-button" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </span>
  </div>
</div>

  
<div class="clone-url "
  data-protocol-type="subversion"
  data-url="/users/set_protocol?protocol_selector=subversion&amp;protocol_type=clone">
  <h3><span class="text-emphasized">Subversion</span> checkout URL</h3>
  <div class="input-group js-zeroclipboard-container">
    <input type="text" class="input-mini input-monospace js-url-field js-zeroclipboard-target"
           value="https://github.com/winnersky/RepData_PeerAssessment_master" readonly="readonly">
    <span class="input-group-button">
      <button aria-label="Copy to clipboard" class="js-zeroclipboard minibutton zeroclipboard-button" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
    </span>
  </div>
</div>



<p class="clone-options">You can clone with
  <a href="#" class="js-clone-selector" data-protocol="http">HTTPS</a>, <a href="#" class="js-clone-selector" data-protocol="ssh">SSH</a>, or <a href="#" class="js-clone-selector" data-protocol="subversion">Subversion</a>.
  <a href="https://help.github.com/articles/which-remote-url-should-i-use" class="help tooltipped tooltipped-n" aria-label="Get help on which URL is right for you.">
    <span class="octicon octicon-question"></span>
  </a>
</p>


  <a href="github-windows://openRepo/https://github.com/winnersky/RepData_PeerAssessment_master" class="minibutton sidebar-button" title="Save winnersky/RepData_PeerAssessment_master to your computer and use it in GitHub Desktop." aria-label="Save winnersky/RepData_PeerAssessment_master to your computer and use it in GitHub Desktop.">
    <span class="octicon octicon-device-desktop"></span>
    Clone in Desktop
  </a>

                <a href="/winnersky/RepData_PeerAssessment_master/archive/master.zip"
                   class="minibutton sidebar-button"
                   aria-label="Download the contents of winnersky/RepData_PeerAssessment_master as a zip file"
                   title="Download the contents of winnersky/RepData_PeerAssessment_master as a zip file"
                   rel="nofollow">
                  <span class="octicon octicon-cloud-download"></span>
                  Download ZIP
                </a>
              </div>
        </div><!-- /.repository-sidebar -->

        <div id="js-repo-pjax-container" class="repository-content context-loader-container" data-pjax-container>
          

<a href="/winnersky/RepData_PeerAssessment_master/blob/0efed7c5ef34373d4b7ee04af10911a752b59019/RepData_PeerAssessment1-master/PA1_template.md" class="hidden js-permalink-shortcut" data-hotkey="y">Permalink</a>

<!-- blob contrib key: blob_contributors:v21:3b0f528808ebd286c5e8395d0cf0d681 -->

<div class="file-navigation js-zeroclipboard-container">
  
<div class="select-menu js-menu-container js-select-menu left">
  <span class="minibutton select-menu-button js-menu-target css-truncate" data-hotkey="w"
    data-master-branch="master"
    data-ref="master"
    title="master"
    role="button" aria-label="Switch branches or tags" tabindex="0" aria-haspopup="true">
    <span class="octicon octicon-git-branch"></span>
    <i>branch:</i>
    <span class="js-select-button css-truncate-target">master</span>
  </span>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax aria-hidden="true">

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <span class="select-menu-title">Switch branches/tags</span>
        <span class="octicon octicon-x js-menu-close" role="button" aria-label="Close"></span>
      </div> <!-- /.select-menu-header -->

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" class="js-select-menu-tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" class="js-select-menu-tab">Tags</a>
            </li>
          </ul>
        </div><!-- /.select-menu-tabs -->
      </div><!-- /.select-menu-filters -->

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <div class="select-menu-item js-navigation-item selected">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/winnersky/RepData_PeerAssessment_master/blob/master/RepData_PeerAssessment1-master/PA1_template.md"
                 data-name="master"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="master">master</a>
            </div> <!-- /.select-menu-item -->
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div> <!-- /.select-menu-list -->

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div> <!-- /.select-menu-list -->

    </div> <!-- /.select-menu-modal -->
  </div> <!-- /.select-menu-modal-holder -->
</div> <!-- /.select-menu -->

  <div class="button-group right">
    <a href="/winnersky/RepData_PeerAssessment_master/find/master"
          class="js-show-file-finder minibutton empty-icon tooltipped tooltipped-s"
          data-pjax
          data-hotkey="t"
          aria-label="Quickly jump between files">
      <span class="octicon octicon-list-unordered"></span>
    </a>
    <button aria-label="Copy file path to clipboard" class="js-zeroclipboard minibutton zeroclipboard-button" data-copied-hint="Copied!" type="button"><span class="octicon octicon-clippy"></span></button>
  </div>

  <div class="breadcrumb js-zeroclipboard-target">
    <span class='repo-root js-repo-root'><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/winnersky/RepData_PeerAssessment_master" class="" data-branch="master" data-direction="back" data-pjax="true" itemscope="url"><span itemprop="title">RepData_PeerAssessment_master</span></a></span></span><span class="separator">/</span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/winnersky/RepData_PeerAssessment_master/tree/master/RepData_PeerAssessment1-master" class="" data-branch="master" data-direction="back" data-pjax="true" itemscope="url"><span itemprop="title">RepData_PeerAssessment1-master</span></a></span><span class="separator">/</span><strong class="final-path">PA1_template.md</strong>
  </div>
</div>


  <div class="commit file-history-tease">
    <div class="file-history-tease-header">
        <img alt="john kim" class="avatar" data-user="4812870" height="24" src="https://avatars1.githubusercontent.com/u/4812870?v=3&amp;s=48" width="24" />
        <span class="author"><a href="/winnersky" rel="author">winnersky</a></span>
        <time datetime="2014-06-12T09:20:34Z" is="relative-time">Jun 12, 2014</time>
        <div class="commit-title">
            <a href="/winnersky/RepData_PeerAssessment_master/commit/0efed7c5ef34373d4b7ee04af10911a752b59019" class="message" data-pjax="true" title="RepData

Reproducible Research Peer Assessments">RepData</a>
        </div>
    </div>

    <div class="participation">
      <p class="quickstat">
        <a href="#blob_contributors_box" rel="facebox">
          <strong>1</strong>
           contributor
        </a>
      </p>
      
    </div>
    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list">
          <li class="facebox-user-list-item">
            <img alt="john kim" data-user="4812870" height="24" src="https://avatars1.githubusercontent.com/u/4812870?v=3&amp;s=48" width="24" />
            <a href="/winnersky">winnersky</a>
          </li>
      </ul>
    </div>
  </div>

<div class="file-box">
  <div class="file">
    <div class="meta clearfix">
      <div class="info file-name">
          <span>187 lines (125 sloc)</span>
          <span class="meta-divider"></span>
        <span>4.167 kb</span>
      </div>
      <div class="actions">
        <div class="button-group">
          <a href="/winnersky/RepData_PeerAssessment_master/raw/master/RepData_PeerAssessment1-master/PA1_template.md" class="minibutton " id="raw-url">Raw</a>
            <a href="/winnersky/RepData_PeerAssessment_master/blame/master/RepData_PeerAssessment1-master/PA1_template.md" class="minibutton js-update-url-with-hash">Blame</a>
          <a href="/winnersky/RepData_PeerAssessment_master/commits/master/RepData_PeerAssessment1-master/PA1_template.md" class="minibutton " rel="nofollow">History</a>
        </div><!-- /.button-group -->

          <a class="octicon-button tooltipped tooltipped-nw"
             href="github-windows://openRepo/https://github.com/winnersky/RepData_PeerAssessment_master?branch=master&amp;filepath=RepData_PeerAssessment1-master%2FPA1_template.md" aria-label="Open this file in GitHub for Windows">
              <span class="octicon octicon-device-desktop"></span>
          </a>

              <a class="octicon-button tooltipped tooltipped-n js-update-url-with-hash"
                 aria-label="Clicking this button will fork this project so you can edit the file"
                 href="/winnersky/RepData_PeerAssessment_master/edit/master/RepData_PeerAssessment1-master/PA1_template.md"
                 data-method="post" rel="nofollow"><span class="octicon octicon-pencil"></span></a>

            <a class="octicon-button danger tooltipped tooltipped-s"
               href="/winnersky/RepData_PeerAssessment_master/delete/master/RepData_PeerAssessment1-master/PA1_template.md"
               aria-label="Fork this project and delete file"
               data-method="post" data-test-id="delete-blob-file" rel="nofollow">
          <span class="octicon octicon-trashcan"></span>
        </a>
      </div><!-- /.actions -->
    </div>
    
  <div id="readme" class="blob instapaper_body">
    <article class="markdown-body entry-content" itemprop="mainContentOfPage"><h1>
<a id="user-content-reproducible-research-peer-assessment-1" class="anchor" href="#reproducible-research-peer-assessment-1" aria-hidden="true"><span class="octicon octicon-link"></span></a>Reproducible Research: Peer Assessment 1</h1>

<p>by Fabian Linzberger<br>
github repo with RMarkdown source code:
<a href="https://github.com/lefant/RepData_PeerAssessment1">https://github.com/lefant/RepData_PeerAssessment1</a><br>
online rendered version on github pages:
<a href="http://lefant.net/RepData_PeerAssessment1/PA1_template.html">http://lefant.net/RepData_PeerAssessment1/PA1_template.html</a>  </p>

<h2>
<a id="user-content-loading-and-preprocessing-the-data" class="anchor" href="#loading-and-preprocessing-the-data" aria-hidden="true"><span class="octicon octicon-link"></span></a>Loading and preprocessing the data</h2>

<div class="highlight highlight-r"><pre>unzip(<span class="pl-s1"><span class="pl-pds">"</span>activity.zip<span class="pl-pds">"</span></span>)
<span class="pl-vo">activity</span> <span class="pl-k">&lt;-</span> read.csv(<span class="pl-s1"><span class="pl-pds">"</span>activity.csv<span class="pl-pds">"</span></span>)</pre></div>

<h2>
<a id="user-content-what-is-the-mean-total-number-of-steps-taken-per-day" class="anchor" href="#what-is-the-mean-total-number-of-steps-taken-per-day" aria-hidden="true"><span class="octicon octicon-link"></span></a>What is the mean total number of steps taken per day?</h2>

<ol class="task-list">
<li>Make a histogram of the total number of steps taken each day</li>
</ol>

<div class="highlight highlight-r"><pre><span class="pl-vo">steps.date</span> <span class="pl-k">&lt;-</span> aggregate(<span class="pl-vo">steps</span> <span class="pl-k">~</span> <span class="pl-vo">date</span>, <span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-vo">activity</span>, <span class="pl-v">FUN</span> <span class="pl-k">=</span> <span class="pl-vo">sum</span>)
barplot(<span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">steps</span>, <span class="pl-v">names.arg</span> <span class="pl-k">=</span> <span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">date</span>, <span class="pl-v">xlab</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>date<span class="pl-pds">"</span></span>, <span class="pl-v">ylab</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>steps<span class="pl-pds">"</span></span>)</pre></div>

<p><a href="/winnersky/RepData_PeerAssessment_master/blob/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-2.png" target="_blank"><img src="/winnersky/RepData_PeerAssessment_master/raw/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-2.png" alt="plot of chunk unnamed-chunk-2" style="max-width:100%;"></a> </p>

<ol class="task-list">
<li>Calculate and report the <strong>mean</strong> and <strong>median</strong> total number of
steps taken per day</li>
</ol>

<div class="highlight highlight-r"><pre>mean(<span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">steps</span>)</pre></div>

<pre><code>## [1] 10766
</code></pre>

<div class="highlight highlight-r"><pre>median(<span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">steps</span>)</pre></div>

<pre><code>## [1] 10765
</code></pre>

<h2>
<a id="user-content-what-is-the-average-daily-activity-pattern" class="anchor" href="#what-is-the-average-daily-activity-pattern" aria-hidden="true"><span class="octicon octicon-link"></span></a>What is the average daily activity pattern?</h2>

<ol class="task-list">
<li>Make a time series plot (i.e. <code>type = "l"</code>) of the 5-minute
interval (x-axis) and the average number of steps taken, averaged
across all days (y-axis)</li>
</ol>

<div class="highlight highlight-r"><pre><span class="pl-vo">steps.interval</span> <span class="pl-k">&lt;-</span> aggregate(<span class="pl-vo">steps</span> <span class="pl-k">~</span> <span class="pl-vo">interval</span>, <span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-vo">activity</span>, <span class="pl-v">FUN</span> <span class="pl-k">=</span> <span class="pl-vo">mean</span>)
plot(<span class="pl-vo">steps.interval</span>, <span class="pl-v">type</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>l<span class="pl-pds">"</span></span>)</pre></div>

<p><a href="/winnersky/RepData_PeerAssessment_master/blob/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-4.png" target="_blank"><img src="/winnersky/RepData_PeerAssessment_master/raw/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-4.png" alt="plot of chunk unnamed-chunk-4" style="max-width:100%;"></a> </p>

<ol class="task-list">
<li>Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?</li>
</ol>

<div class="highlight highlight-r"><pre><span class="pl-vo">steps.interval</span><span class="pl-k">$</span><span class="pl-vo">interval</span>[which.max(<span class="pl-vo">steps.interval</span><span class="pl-k">$</span><span class="pl-vo">steps</span>)]</pre></div>

<pre><code>## [1] 835
</code></pre>

<h2>
<a id="user-content-imputing-missing-values" class="anchor" href="#imputing-missing-values" aria-hidden="true"><span class="octicon octicon-link"></span></a>Imputing missing values</h2>

<ol class="task-list">
<li>Calculate and report the total number of missing values in the
dataset (i.e. the total number of rows with <code>NA</code>s)</li>
</ol>

<div class="highlight highlight-r"><pre>sum(is.na(<span class="pl-vo">activity</span>))</pre></div>

<pre><code>## [1] 2304
</code></pre>

<ol class="task-list">
<li>Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For
example, you could use the mean/median for that day, or the mean
for that 5-minute interval, etc.</li>
</ol>

<p>I will use the means for the 5-minute intervals as fillers for missing
values.</p>

<ol class="task-list">
<li>Create a new dataset that is equal to the original dataset but with
the missing data filled in.</li>
</ol>

<div class="highlight highlight-r"><pre><span class="pl-vo">activity</span> <span class="pl-k">&lt;-</span> merge(<span class="pl-vo">activity</span>, <span class="pl-vo">steps.interval</span>, <span class="pl-v">by</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>interval<span class="pl-pds">"</span></span>, <span class="pl-v">suffixes</span> <span class="pl-k">=</span> c(<span class="pl-s1"><span class="pl-pds">"</span><span class="pl-pds">"</span></span>, 
    <span class="pl-s1"><span class="pl-pds">"</span>.y<span class="pl-pds">"</span></span>))
<span class="pl-vo">nas</span> <span class="pl-k">&lt;-</span> is.na(<span class="pl-vo">activity</span><span class="pl-k">$</span><span class="pl-vo">steps</span>)
<span class="pl-vo">activity</span><span class="pl-k">$</span><span class="pl-vo">steps</span>[<span class="pl-vo">nas</span>] <span class="pl-k">&lt;-</span> <span class="pl-vo">activity</span><span class="pl-k">$</span><span class="pl-vo">steps.y</span>[<span class="pl-vo">nas</span>]
<span class="pl-vo">activity</span> <span class="pl-k">&lt;-</span> <span class="pl-vo">activity</span>[, c(<span class="pl-c1">1</span><span class="pl-k">:</span><span class="pl-c1">3</span>)]</pre></div>

<ol class="task-list">
<li>Make a histogram of the total number of steps taken each day and
Calculate and report the <strong>mean</strong> and <strong>median</strong> total number of
steps taken per day. Do these values differ from the estimates from
the first part of the assignment? What is the impact of imputing
missing data on the estimates of the total daily number of steps?</li>
</ol>

<div class="highlight highlight-r"><pre><span class="pl-vo">steps.date</span> <span class="pl-k">&lt;-</span> aggregate(<span class="pl-vo">steps</span> <span class="pl-k">~</span> <span class="pl-vo">date</span>, <span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-vo">activity</span>, <span class="pl-v">FUN</span> <span class="pl-k">=</span> <span class="pl-vo">sum</span>)
barplot(<span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">steps</span>, <span class="pl-v">names.arg</span> <span class="pl-k">=</span> <span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">date</span>, <span class="pl-v">xlab</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>date<span class="pl-pds">"</span></span>, <span class="pl-v">ylab</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>steps<span class="pl-pds">"</span></span>)</pre></div>

<p><a href="/winnersky/RepData_PeerAssessment_master/blob/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-8.png" target="_blank"><img src="/winnersky/RepData_PeerAssessment_master/raw/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-8.png" alt="plot of chunk unnamed-chunk-8" style="max-width:100%;"></a> </p>

<div class="highlight highlight-r"><pre>mean(<span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">steps</span>)</pre></div>

<pre><code>## [1] 10766
</code></pre>

<div class="highlight highlight-r"><pre>median(<span class="pl-vo">steps.date</span><span class="pl-k">$</span><span class="pl-vo">steps</span>)</pre></div>

<pre><code>## [1] 10766
</code></pre>

<p>The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.</p>

<h2>
<a id="user-content-are-there-differences-in-activity-patterns-between-weekdays-and-weekends" class="anchor" href="#are-there-differences-in-activity-patterns-between-weekdays-and-weekends" aria-hidden="true"><span class="octicon octicon-link"></span></a>Are there differences in activity patterns between weekdays and weekends?</h2>

<ol class="task-list">
<li>Create a new factor variable in the dataset with two levels --
"weekday" and "weekend" indicating whether a given date is a
weekday or weekend day.</li>
</ol>

<div class="highlight highlight-r"><pre><span class="pl-en">daytype</span> <span class="pl-k">&lt;-</span> <span class="pl-k">function</span>(<span class="pl-vo">date</span>) {
    <span class="pl-k">if</span> (weekdays(as.Date(<span class="pl-vo">date</span>)) <span class="pl-k">%in%</span> c(<span class="pl-s1"><span class="pl-pds">"</span>Saturday<span class="pl-pds">"</span></span>, <span class="pl-s1"><span class="pl-pds">"</span>Sunday<span class="pl-pds">"</span></span>)) {
        <span class="pl-s1"><span class="pl-pds">"</span>weekend<span class="pl-pds">"</span></span>
    } <span class="pl-k">else</span> {
        <span class="pl-s1"><span class="pl-pds">"</span>weekday<span class="pl-pds">"</span></span>
    }
}
<span class="pl-vo">activity</span><span class="pl-k">$</span><span class="pl-vo">daytype</span> <span class="pl-k">&lt;-</span> as.factor(sapply(<span class="pl-vo">activity</span><span class="pl-k">$</span><span class="pl-vo">date</span>, <span class="pl-vo">daytype</span>))</pre></div>

<ol class="task-list">
<li>Make a panel plot containing a time series plot (i.e. <code>type = "l"</code>)
of the 5-minute interval (x-axis) and the average number of steps
taken, averaged across all weekday days or weekend days
(y-axis).</li>
</ol>

<div class="highlight highlight-r"><pre>par(<span class="pl-v">mfrow</span> <span class="pl-k">=</span> c(<span class="pl-c1">2</span>, <span class="pl-c1">1</span>))
<span class="pl-k">for</span> (<span class="pl-vo">type</span> <span class="pl-k">in</span> c(<span class="pl-s1"><span class="pl-pds">"</span>weekend<span class="pl-pds">"</span></span>, <span class="pl-s1"><span class="pl-pds">"</span>weekday<span class="pl-pds">"</span></span>)) {
    <span class="pl-vo">steps.type</span> <span class="pl-k">&lt;-</span> aggregate(<span class="pl-vo">steps</span> <span class="pl-k">~</span> <span class="pl-vo">interval</span>, <span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-vo">activity</span>, <span class="pl-v">subset</span> <span class="pl-k">=</span> <span class="pl-vo">activity</span><span class="pl-k">$</span><span class="pl-vo">daytype</span> <span class="pl-k">==</span> 
        <span class="pl-vo">type</span>, <span class="pl-v">FUN</span> <span class="pl-k">=</span> <span class="pl-vo">mean</span>)
    plot(<span class="pl-vo">steps.type</span>, <span class="pl-v">type</span> <span class="pl-k">=</span> <span class="pl-s1"><span class="pl-pds">"</span>l<span class="pl-pds">"</span></span>, <span class="pl-v">main</span> <span class="pl-k">=</span> <span class="pl-vo">type</span>)
}</pre></div>

<p><a href="/winnersky/RepData_PeerAssessment_master/blob/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-10.png" target="_blank"><img src="/winnersky/RepData_PeerAssessment_master/raw/master/RepData_PeerAssessment1-master/figure/unnamed-chunk-10.png" alt="plot of chunk unnamed-chunk-10" style="max-width:100%;"></a> </p>
</article>
  </div>

  </div>
</div>

<a href="#jump-to-line" rel="facebox[.linejump]" data-hotkey="l" style="display:none">Jump to Line</a>
<div id="jump-to-line" style="display:none">
  <form accept-charset="UTF-8" class="js-jump-to-line-form">
    <input class="linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" autofocus>
    <button type="submit" class="button">Go</button>
  </form>
</div>

        </div>

      </div><!-- /.repo-container -->
      <div class="modal-backdrop"></div>
    </div><!-- /.container -->
  </div><!-- /.site -->


    </div><!-- /.wrapper -->

      <div class="container">
  <div class="site-footer" role="contentinfo">
    <ul class="site-footer-links right">
      <li><a href="https://status.github.com/">Status</a></li>
      <li><a href="https://developer.github.com">API</a></li>
      <li><a href="http://training.github.com">Training</a></li>
      <li><a href="http://shop.github.com">Shop</a></li>
      <li><a href="/blog">Blog</a></li>
      <li><a href="/about">About</a></li>

    </ul>

    <a href="/" aria-label="Homepage">
      <span class="mega-octicon octicon-mark-github" title="GitHub"></span>
    </a>

    <ul class="site-footer-links">
      <li>&copy; 2014 <span title="0.04702s from github-fe125-cp1-prd.iad.github.net">GitHub</span>, Inc.</li>
        <li><a href="/site/terms">Terms</a></li>
        <li><a href="/site/privacy">Privacy</a></li>
        <li><a href="/security">Security</a></li>
        <li><a href="/contact">Contact</a></li>
    </ul>
  </div><!-- /.site-footer -->
</div><!-- /.container -->


    <div class="fullscreen-overlay js-fullscreen-overlay" id="fullscreen_overlay">
  <div class="fullscreen-container js-suggester-container">
    <div class="textarea-wrap">
      <textarea name="fullscreen-contents" id="fullscreen-contents" class="fullscreen-contents js-fullscreen-contents" placeholder=""></textarea>
      <div class="suggester-container">
        <div class="suggester fullscreen-suggester js-suggester js-navigation-container"></div>
      </div>
    </div>
  </div>
  <div class="fullscreen-sidebar">
    <a href="#" class="exit-fullscreen js-exit-fullscreen tooltipped tooltipped-w" aria-label="Exit Zen Mode">
      <span class="mega-octicon octicon-screen-normal"></span>
    </a>
    <a href="#" class="theme-switcher js-theme-switcher tooltipped tooltipped-w"
      aria-label="Switch themes">
      <span class="octicon octicon-color-mode"></span>
    </a>
  </div>
</div>



    <div id="ajax-error-message" class="flash flash-error">
      <span class="octicon octicon-alert"></span>
      <a href="#" class="octicon octicon-x flash-close js-ajax-error-dismiss" aria-label="Dismiss error"></a>
      Something went wrong with that request. Please try again.
    </div>


      <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/frameworks-153d6254b838872c8db73c8bd92905913f6f5b2164b7e40e5292286bd5a39403.js" type="text/javascript"></script>
      <script async="async" crossorigin="anonymous" src="https://assets-cdn.github.com/assets/github-f4947a80dc89b7b7941d65654d67ea6d87fb3f3baf28a2975462979455f8dcbe.js" type="text/javascript"></script>
      
      
  </body>
</html>

