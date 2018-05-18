![image](https://github.com/Shirakumo/radiance/blob/master/static/radiance.png?raw=true)

## Translation Progress

* ~~Radianceについて 2~~ 

* ~~入手するには 3~~

* ~~利用法 3~~

* ~~簡単な例 20~~

1. Radianceのコンセプトと部品

   ~~1.1 URI 10~~

   ~~1.2 リクエストとレスポンス 15~~
   
   ~~1.3 ルーティング 20~~
   
   1.4 URIディスパッチャー 8

   1.5 Page 12

   1.6 APIエンドポイント 35

   1.7 オプション 12

   1.8 モジュール 10

   1.9 フック 10

   1.10 インターフェイス 30

   1.11 環境 13

   1.12 インスタンスの管理 10

2. ~~標準のインターフェイス 6~~

   ~~2.1 管理者権限(admin) 8~~

   ~~2.2 auth 6~~

   ~~2.3 禁止(ban) 3~~

   ~~2.4 キャッシュ 8~~

   ~~2.5 データベース 15~~

   ~~2.6 logger 3~~

   ~~2.7 メール 4~~

   ~~2.8 プロファイル 7~~

   ~~2.9 rate 5~~

   ~~2.10 サーバ 7~~

   ~~2.11 セッション 5~~

   ~~2.12 ユーザ 4~~
  

## Radianceについて

Radianceは、Webアプリケーションの開発環境です。Webフレームワークのようなものですが、より汎用的で、変更も簡単です。 特別な変更を加える必要なく、個人的なWebサイトやWebアプリを、簡単に書くことができます。

## 入手するには

Radianceと関連するモジュールやアプリケーションは、Quicklispを通して配布されています。Radianceをインストールするには次のようにしてください:

```lisp
(ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
(ql:quickload :radiance)
```

この後では、Quicklispの`quickload`コマンドで、[Purplish](https://github.com/Shirakumo/purplish)等のRadianceモジュールを読み込んで利用できるようになります。

## 利用法

Radianceのチュートリアルは[こちら](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md)です。Radianceの重要なコンセプトや、Webアプリを書く方法を紹介しています。このチュートリアルでは、Radianceの大まかな使い方に親しみ、ある特定の機能が必要なとき、どこを調べるべきかの道しるべになるでしょう。チュートリアルの最後では、本番環境での、Radianceのインストールとデプロイ方法について説明します。

## 簡単な例

おそらく、あなたが一番したいことは、HTMLをサーブすることでしょう。では、その方向に進めながら、少しずつ拡張していきましょう。はじめる前に、Radianceの準備をしましょう。

```common-lisp
(ql:quickload :radiance)
(radiance:startup)
```

Radianceを使うのが初めての場合は、`r-welcome`モジュールを使いながら進めましょう。`r-welcome`モジュールを使うと、ブラウザーで開くことができる最初のページへのリンクをつけることができます。まずは、小さなページをリンクづけましょう。

```common-lisp
(in-package :rad-user)

(define-page example "/example" ()
  (setf (content-type *response*) "text/plain") 
  "Hi!")
```

[localhost:8080/example](http://localhost:8080/example)にアクセスすると、"Hi"と表示されるはずです。
これでは、かなり退屈ですね。
代わりにHTMLを出してみましょう。
まずは、[cl-who](http://weitz.de/cl-who/)を使います。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

普通であれば、コードを書き直しながら、フォントの書式を変えたり、CSSファイルを追加したりしながら、スタイルを作っていきます。
確かにCSSファイルをサーブすることは可能ですが、長い目でみれば、それは最適の方法とはいえません。

代わりに、モジュール(module)を作る方法を紹介します。
モジュールを使うことで、Webページを適切に整理することができます。
モジュール(module)のためのファイルは、手動で作ることができますが、ここではRadianceの雛形を用いて、モジュールを自動生成しましょう。

```common-lisp
(create-module "example")
```

モジュールを自動生成すると、モジュールがあるパスを返します。
生成されるものは、ASDFのシステムファイル、mainのlispファイル、`static`と`template`という名前のフォルダ２つです。

`static`フォルダには静的にサーブされるファイルが入り、`template`にはテンプレートエンジン関連のファイルが入ります。

`example.lisp`を開いて、先ほどの例で使ったコードを引き継いでページを定義しましょう。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```
ページは、シンボル名で特定されます。
自分のモジュールを作ることでパッケージを持つことになります。
先ほどの例のシンボルは、以前に一度も使われていないものです。
名前の衝突を避けるために、`rad-user`パッケージでページを消す必要があるかもしれません。

```common-lisp
(remove-page 'rad-user::example)
```

次に、簡単なCSSファイルを作りましょう。
`static`フォルダに、`example.css`という名前で配置しましょう。
自分で書くのが面倒であれば、次のCSSコードを使ってください。

```CSS
body{
    font-family: sans-serif;
    font-size: 12pt;
    background: #EEE;
}

header{
    text-align: center;
}

main{
    width: 800px;
    margin: 0 auto 0 auto;
    background: #FFF;
    padding: 10px;
    border: 1px solid #BBB;
    border-radius: 5px;
}
```
では、CSSファイルにリンクするように、HTMLを修正しましょう。
アドレスがスタイルシートにたどり着くために、後に、Radianceのルーティングシステムを使う必要が出てきます。
今はその必要はないので、あせらないでください。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page")
             (:link :rel "stylesheet" :type "text/css" 
                    :href (uri-to-url "/static/example/example.css" :representation :external)))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

ページを再読み込みすると、スタイルが適用されているはずです。
`uri-to-url`が動作する原理について詳しく知りたくなると思いますが、それは後ほど説明します。
要は、どのようにセットアップしても、静的なファイルへのリンクは、適切に解決されるということです。

## 1. Radianceのコンセプトと部品
### 1.1 URI

Radianceの中心的な概念は`URI`です。
URIはオブジェクトであり、ドメイン、ポート番号(オプション)、パスを含むリストで構成されます。

RadianceのURIは、一般的なURIから要素を抽出したものなので、スキーマやクエリ、フラグメント等は含みません。
また、重要な違いとしては、`domains`のURIは、フレームワーク全体の複数のポイントで使われることであり、
`location`を捕捉するときや、`dispatch matching`を処理するためにも使われます。

URIは、変更可能であり。URIの修正は、[クリティカルパス](https://www.weblio.jp/content/critical+path)がある場所で行われるはずなので、パフォーマンス上重要です。予想されない方法でURIを修正した場合、予期しない動作につながる可能性があります。

URIは、単一の文字列で表現されて、文字列にシリアライズされて、完全なURIのオブジェクトにパースして戻すことも可能です。
URIは、FASLファイルにリテラルとして書き出されるので、マクロから吐き出しても大丈夫です。
URIのシンタックスは、次の通りです。

```BNF
URI     ::= DOMAINS? (':' PORT)? '/' PATH?
DOMAINS ::= DOMAIN ('.' DOMAIN)*
DOMAIN  ::= ('a'..'Z' | '0'..'9' | '-')
PORT    ::= ('0'..'9'){1, 5}
PATH    ::= .*
```

URIをURLに変換するには、`uri-to-url`が使えます。`uri-to-url`を使うと、反転(reversal)、エンコーディング(encoding)、体裁直しは自動で行われます。

`uri`, `domains`, `port`, `path`, `matcher`, `uri-string`, `make-uri`, `make-url`, `ensure-uri`, `copy-uri`, `parse-uri`, `uri<`, `uri>`, `uri=`, `uri-matches`, `merge-uris`, `represent-uri`, `uri-to-url`を参考にしてください。

### 1.2 リクエストとレスポンス

行き来するデータを格納しておくために、`request`オブジェクトと`response`オブジェクトを作ることにしました。
`request`オブジェクトは、リクエストがどこに向かうのかを合わすURI、また、POST、GET、ヘッダ、クッキーなどのHTTP通信のペイロードデータを全て保持します。
`response`オブジェクトは、リターンコード、ヘッダー、クッキー、実際のBODYデータを含みます。

リクエストが行われている間、これらの2つのオブジェクトが必ず存在し、`*request*`と`*response*`に束縛されなければいけません。
それらは、動的なページを生成するために必要な重要情報を多く格納しています。
さらに、リクエストは`data`テーブルを含んでおり、任意のデータを保持することができます。
これは、システム内の各々の部品の間で、リクエストの実行中に取得されるような情報をやりとりをするときに役立ちます。

リクエストは、必ずしもHTTPサーバからくる必要はありません。
動作をテストするためには、プログラムからリクエストを送ることも可能です。
どのような場合であっても、リクエストをディスパッチするインターフェイスは、`request`と呼ばれます。
この仕組みは、リクエストとレスポンスを構築して、URIを適切に処理します。
もし自分自身でリクエストオブジェクトを送りたいのであれば、`execute-request`を使うこともできます。

実際にリクエストを処理する方法に関する詳しい情報は、dispatcher、pages、API endpointをご参照ください。

`*request*`, `*response*`, `*default-external-format*`, `*default-content-type*  `, `request`, `uri`, `http-method`, `headers`, `post-data`, `get-data`, `cookies`, `user-agent`, `referer`, `domain`, `remote`, `data`, `issue-time`, `response`, `data`, `return-code`, `content-type`, `external-format`, `headers`, `cookies`, `cookie`, `name`, `value`, `domain`, `path`, `expires`, `http-only`, `secure`, `cookie-header`, `cookie`, `get-var`, `post-var`, `post/get`, `header`, `file`, `redirect`, `serve-file`, `request-run-time`, `*debugger*`, `handle-condition`, `render-error-page`, `execute-request`, `set-data`, `request`も参考にしてしてください。

### 1.3 ルーティング
リクエストがディスパッチされる前には、ルーティングシステムを通過します。
他のフレームワークでは'routes'はどのハンドラがリクエストを処理するかを指定しますが、Radianceではその方式とは違います。
Radianceにおいてルート(`route`)とは、URI変換の様態です。
システムのこの部分は、２つの**世界**を作成して保持します。**内部の世界**と**外部の世界**です。

**内部の世界**は、実際にWebアプリケーションが住む世界です。
**外部の世界**は、HTTPサーバとWebサイトを利用するユーザが住む世界です。
この区別は、あるサーバにおける潜在的な罠を避けてWebアプロケーションを書くために、必要不可欠です。
あなたのアプリケーションを動作させるために、どのようなドメインやポート、パスが必要になるか、心配する必要がありません。
一方で、Webの管理者として、アプリが壊れないように、システムをあなたの望み通りにカスタマイズして動作させる必要があります。

そのために、ルーティングが役に立ちます。ルーティングには、`Mapping`と`Reversal`の2種類があります:
Mappingは、外部の世界からくるURIを内部の世界のURIに変換します。
普通は、トップレベルのドメインを切り取り、サブドメインにマッピングします。
Reversalは逆のことをします。内部の世界から外部の世界へといきます。
このことは、あなたが提供するWebページが、実際に外部からアクセス可能なリソースを参照できるようにするために、欠かせません。

ルーティングは、任意の処理を行うことができます。
基本的なレベルでは、何らかの方法でURIを修正する関数です。
この利用法では、管理者として、あなたの期待に十分応える強力で柔軟なシステムを構築することができます。
アプリケーションの開発者として、`external-uri`か`uri-to-url`を、ページ内に置く全てのリンクに使うようにしてください。

`route`, `name`, `direction`, `priority`, `translator`, `route`, `remove-route`, `list-routes`, `define-route`, `define-matching-route`, `define-target-route`, `define-string-route`, `internal-uri`, `external-uri`も参考にしてください。

### 1.4 URIディスパッチャー
Finally we come to the part that actually generates content for a request. 
URI dispatchers are a subclass of URI that also carry a name, a function, and a priority. 
The live in a priority-sorted list, 
which is processed whenever a request arrives. 
The Request's URI is matched against each dispatcher. 
The function of the first dispatcher that matches is then executed.

And that's it. The dispatcher's function is responsible for setting the necessary values in the Response object to deliver the page content. In order to do this it can either directly set the `data` field of the Response object, or you can return an appropriate value from the function. Radiance only accepts four types of values: `stream`, `pathname`, `string`, and `(array (unsigned-byte 8))`.

If a URI dispatcher does not have an explicit priority number, 
its priority over others is determined by the specificity of the URI. 
See the URI sorting function `uri>` for an explanation on how exactly this is calculated.

See `uri-dispatcher`, `name`, `dispatch-function`, `priority`, `uri-dispatcher`, `remove-uri-dispatcher`, `list-uri-dispatchers`, `uri-dispatcher>`, `define-uri-dispatcher`, `dispatch`

### 1.5 Page
Pages are what you will likely use to define your actual content serving functions. 
However, a page is just a uri-dispatcher with some extra functionality in the definition macro that makes things easier on you. Most notably are the extensible options, for which you can find an explanation below.

There are a couple of default pages set up by Radiance itself. 
First there's the `favicon` and `robots` pages, which simply serve the respective files from Radiance's `static/` directory. 
You'll probably want to either provide your own pages for that or update the files on your production server.

Then there's the `static` page, which is responsible for serving static contents for all web applications and modules. 
It should be active on any domain and always on the path `/static/...` where `...` must have a form where the first directory is the name of a module, 
and the rest is a path within that module's `static/` directory. 
This allows you to always be able to refer to static files like CSS, JS, and images through a common path.

Finally there's the `api` page, which is responsible for handling the dispatch of API endpoints, 
which are explained in the following section. 
The page acts similarly to the static one by capturing the `/api/...` path on all domains.

See `page`, `remove-page`, `define-page`

### 1.6 APIエンドポイント
Radiance provides integrated support for REST API definition. 
This is not just a tacked-on feature, but rather because most modern applications want to provide an API of some kind, 
and because Radiance advises a certain way of writing your applications that necessarily involves API endpoints.

Conceptually, API endpoints are functions that are callable through a browser request. 
Their response is then serialised to a format that is readable by the requester, 
whatever that may be. 
Important to remember however is that API endpoints should be usable by both users and programs. 
Radiance encourages this because usually any kind of action that can be performed programmatically 
through an API will also have to be performed by the user in some way. 
In order to avoid duplication, the two can be conflated.

As such, usually any kind of data modification action should be provided through an API endpoint that reacts slightly differently depending on whether a user or an application requests it. 
In the case of a user, it should usually redirect back to an appropriate page, 
and in the case of an application it should provide a data payload in a readable format.

The first part of all of this is the API format system, 
which is responsible for serialising data to some specified format. 
By default only an S-expression based format is supplied, 
but a contrib to get JSON output can easily be loaded.

The second part is the specification of the `browser` POST/GET parameter. 
If that parameter contains the exact string `"true"`, 
then the API request is treated as coming from a user, 
and thus a redirect rather than a data payload should be outputted.

Your application should make use of those things in order to provide a properly integrated api. 
Now, an actual endpoint definition is composed of a name, a raw function, a lambda-list describing the arguments of the function, and a request parsing function. 
Typically for your arguments, only required and optional arguments make sense. 
After all, an HTTP request only has "keyword arguments" that it can provide, and those can either be present or missing.

The name of an API endpoint also serves as the identifier that tells you where you can reach it. 
API endpoints live on the `/api/` path, followed by the name of the endpoint. 
As such, you are responsible for prefixing your endpoints with the name of your module or application in order to avoid accidentally tripping over other endpoints. 
This is unlike in uri dispatchers, because API endpoints have to match exactly and don't allow any ambiguity or processing of the path. 
Thus every endpoint must have a unique path, which can also immediately serve as the name.

The raw function is the function that the API provides an interface for. 
It is responsible for performing the requested action and returning the appropriate data as described above. 
For returning formatted API data, see `api-output`. 
For redirecting in the case of a browser request, see `redirect`.

Finally, the request parsing function is responsible for taking a Request object, extracting the arguments the function needs from it, and finally calling that function with the appropriate arguments-- if possible. 
The parsing function may signal an `api-argument-missing` error if a required argument is missing. 
Superfluous arguments should be ignored.

You can also programmatically call an API endpoint using `call-api`, or simulate a Request call with `call-api-request`, without having to go through the whole URI dispatch mechanism.

Similarly to pages, API endpoint definitions also accept extensible options that make definition simpler. See the following section for an explanation of options.

See `api`, `*default-api-format*`, `*serialize-fallback*`, `api-format`, `remove-api-format`, `list-api-formats`, `define-api-format`, `api-output`, `api-serialize`, `api-endpoint`, `remove-api-endpoint`, `list-api-endpoints`, `api-endpoint`, `name`, `handler`, `argslist`, `request-handler`, `call-api-request`, `call-api`, `define-api`

### 1.7 オプション
Options are a way of providing an extensible definition macro. 
This is useful when a framework provides a common way of defining something, 
but other parts may want to provide extensions to that in order to make common operations shorter. 
For example, a common task is to restrict a page or API endpoint to people who have the required access credentials.

In order to facilitate this, Radiance provides a rather generic options mechanism. 
Options are divided up by an option type that designates to which definition macro the option belongs. 
Radiance provides the `api` and `page` option types out of the box.

Each option has a keyword for a name and an expander function that must accept a number of arguments, 
depending on the option type. 
Always provided as arguments are the name of the thing being defined, 
the list of body forms of the definition, and a final, optional, value that was provided to the option in the options list, if it was mentioned at all. 
This expansion function is then responsible for transforming the body forms of the definition macro in some way. 
It can also emit a second form that is placed outside of the definition itself, 
in order to allow setting up the environment in some manner.

See `option`, `option-type`, `name`, `expander`, `option`, `remove-option`, `list-options`, `define-option`, `expand-options`

### 1.8 モジュール
The concept of a module is essential to Radiance. It serves as the representation of a "part" of the whole. On a technical level, a module is a package that has special metadata attached to it. 
It is provided by the `modularize` system and is used to facilitate hooks and triggers, interfaces, and the tracking of a few other pieces of information.

What this means for you is that instead of a standard `defpackage` you should use a `define-module` form to define your primary package. 
The syntax is the same as `defpackage`, but includes some extra options like `:domain`, 
which allows you to specify the primary domain on which this module should operate (if any).

The module system also allows the tying of an ASDF system to a module. 
If that is done, then the ASDF system becomes a "virtual module". 
In order to do this, you must add three options to your system definition:

```commonlisp
:defsystem-depends-on (:radiance)
:class "radiance:virtual-module"
:module-name "MY-MODULE"
```

This allows Radiance to identify and associate ASDF system information to your module. 
For automated creation of the necessary system and module definitions for a new module, see `create-module`.

See `virtual-module`, `virtual-module-name`, `define-module`, `define-module-extension`, `delete-module`, `module`, `module-p`, `module-storage`, `module-storage-remove`, `module-identifier`, `module-name`, `current-module`, `module-domain`, `module-permissions`, `module-dependencies`, `module-required-interfaces`, `module-required-systems`, `module-pages`, `module-api-endpoints`, `describe-module`, `find-modules-directory`, `*modules-directory*`, `create-module`

### 1.9 フック
One of the mechanisms that Radiance provides to allow integrating modules into each other is hooks. 
Hooks allow you to run an arbitrary function in response to some kind of event. 
For example, a forum software might set up a hook that is triggered whenever a new post is created. 
An extension could then define a trigger on that hook that performs additional tasks.

A hook can have an arbitrary number of triggers defined on it, 
but you should ensure that a trigger does not take too long, 
as triggering a hook is a blocking operation that won't finish until all of the triggers have completed. 
As such, a long-running trigger operation might delay a request response for too long.

Sometimes hooks should function more like switches, where they can be "on" for a long time, 
until they're turned "off" again later. 
If new triggers are defined during that time, they should be called automatically. 
This is what the `define-hook-switch` facilitates. 
It produces two hooks. Once the first one has been triggered, 
any trigger that is defined on it later is called automatically until the second hook is triggered. 
This allows triggers on hooks like `server-start` to function properly 
even if the trigger is only defined after the server has already been started.

See `list-hooks`, `define-hook`, `remove-hook`, `define-trigger`, `remove-trigger`, `trigger`, `define-hook-switch`

### 1.10 インターフェイス
In order to avoid becoming monolithic, and in order to allow extensible backends, 
Radiance includes an interface system. 
In the most general sense, an interface provides a promise as to how some functions, macros, variables, etc. should work, 
but does not actually implement them. 
The actual functionality that makes everything that the interface outlines work is pushed off to an implementation. 
This allows users to code against an interface and make use of its provided functionality, 
without tying themselves to any particular backend.

For a concrete example, let's say there's an interface for a database. 
This is sensible, since there are many different kinds of databases, 
that all offer many differing ways of interaction, 
but still all also offer some very common operations: storing data, retrieving data, and modifying the data. 
Thus we create an interface that offers these common operations. 
It is then up to an implementation for a specific kind of database to make the actual operations work. 
As an application writer, you can then make use of the database interface, 
and with it, make your application automatically work with lots of different databases.

Aside from giving application writers an advantage, 
the decoupling that the interfaces provide also mean that a system administrator can write their own implementation with relative ease, should their particular requirements not be met by existing implementations. 
Thanks to the opaqueness of the interfaces, an implementation can both provide a bridge to something that runs in the lisp process, and something that is completely external. 
This leaves a lot of choice open for the administrator of a production system to allow them to pick exactly what they need.

In practise, interfaces are special kinds of modules, 
and thus special kinds of packages. 
As part of their definition, they include a series of definitions for other bindings like functions, variables, etc. 
Since it is a package, as a user you can use the interface's components just like you would use anything else in any other package. 
There is no difference. 
As an implementation writer, you then simply redefine all the definitions that the interface outlines.

In order to actually load a module that makes use of an interface, an implementation for the interface has to be loaded beforehand. 
Otherwise, macros could not work properly. 
Thus, in order to allow depending on interfaces in your ASDF system definition without having to refer to a specific implementation, Radiance provides an ASDF extension. 
This extension makes it possible to add a list like `(:interface :foo)` to your `:depends-on` list. 
Radiance will then resolve the interface to a concrete implementation thereof when the module is loaded.

Radiance provides a bunch of standard interfaces. 
Each of those interfaces has at least one standard implementation provided by [radiance-contribs](https://shirakumo.org/projects/radiance-contribs). 
The interfaces are:

* `admin`  
  Provides an extensible administration site.
* `auth`  
  Handles everything about authentication and login.
* `ban`  
  Allows banning users from the site by their IP address.
* `cache`  
  Provides a very simple caching interface.
* `database`  
  A flexible database interface that allows both object-stores and relational databases as backends.
* `logger`  
  A simple logging interface to allow printing debug and information messages.
* `mail`  
  Minimal interface to send emails with.
* `profile`  
  Provides an extensible user profile site and user fields.
* `rate`  
  Allows rate limitation for access to certain resources.
* `server`  
  The interface that bridges to a server to connect Radiance with an external universe.
* `session`  
  Ensures persistent sessions for users to allow tracking them.
* `user`  
  Provides user accounts and permissions.
  
The interfaces are described in-depth below.

See `interface`, `interface-p`, `implementation`, `implements`, `reset-interface`, `define-interface-extension`, `find-implementation`, `load-implementation`, `define-interface`, `define-implement-trigger`

### 1.11 環境
In order to permit running multiple instances of Radiance with different setups on the same machine, 
Radiance provides what it calls an Environment system. 
The Environment is basically the set of configuration files for Radiance itself and all of the loaded modules. 
The Radiance configuration also includes the mapping of interface to chosen implementation 
and thus decides what should be picked if an interface is requested.

The particular environment that is used is chosen at the latest when `startup` is called, and the earliest when a module is loaded. 
In the latter case, interactive restarts are provided to allow you to pick an environment. 
This is necessary, as otherwise Radiance won't be able to resolve the interface mapping.

As part of the environment system, Radiance provides you with a configuration system that you can 
--and probably should-- use for your application. 
It ensures that the settings are properly multiplexed for each environment, 
and that the settings are always persistent. 
It also uses a human-readable storage format, 
such that the files can be read and modified without requiring any special tools.

See [ubiquitous](https://shinmera.github.io/ubiquitous) for the actual handling and use-instructions of the configuration storage. 
Just note that instead of the `value` functions, Radiance provides `config` functions.

See `environment-change`, `environment`, `check-environment`, `mconfig-pathname`, `mconfig-storage`, `mconfig`, `defaulted-mconfig`, `config`, `defaulted-config`

### 1.12 インスタンスの管理
Finally, Radiance provides a standard startup and shutdown sequence that should ensure things are properly setup and readied, and afterwards cleaned up nicely again. 
A large part of that sequence is just ensuring that certain hooks are called in the proper order and at the appropriate times.

While you can start a server manually by using the appropriate interface function, 
you should not expect applications to run properly if you do it that way. 
Many of them will expect certain hooks to be called in order to work properly. 
This is why you should always, 
unless you exactly know what you're doing, use `startup` and `shutdown` to manage a Radiance instance. 
The documentation of the two functions should explain exactly which hooks are triggered and in which order. 
An implementation may provide additional, unspecified definitions on symbols in the interface package, as long as said symbols are not exported.

See `*startup-time*`, `uptime`, `server-start`, `server-ready`, `server-stop`, `server-shutdown`, `startup`, `startup-done`, `shutdown`, `shutdown-done`, `started-p`

## 2. 標準のインターフェイス

インターフェイスは、Radianceとcore packageと一緒に配布されています。
ライブラリは、追加のインターフェイスを提供することも可能です。
インターフェイスの実装ですが、インターフェイス定義では、次の制限の緩和が許可されています:

`&key`引数を含むラムダリストは、実装依存のキーワード引数を使って拡張できます。
`&optional`引数を含み、`&key`か`&rest`を含まないラムダリストは、オプショナル引数で拡張できます。
必須の引数しか含まないラムダリストは、オプショナル引数かキーワード引数で拡張できます。

### 2.1 管理(admin)

このインターフェイスを使うと、管理者ページを作成できます。
ユーザ構成の設定やシステム情報の表示のために使えます。
"管理(administration)"という名前ですが、システムの管理者だけに限ったものではありません。
どのユーザにも利用できることができます。

管理者ページは、カテゴリー分けされたメニュやパネルを表示できなければいけません。
パネル群は、他のモジュールによって提供されており、`admin:define-panel`で追加できます。
秘密情報を含むようなパネルにアクセスするパネルは、`:access`オプションでアクセスを禁止させるようにしてください。

パーミッションについては、 userインターフェイスを参照してください。

管理者ページや特定のパネルにリンクをはるには、`page`リソースを使ってください。

`admin:list-panels`, `admin:remove-panel`, `admin:define-panel`, `admin:panel`をご覧ください。

### 2.2 auth

`認証インターフェイス`は、ユーザをリクエストと結びつけます。
そのために、ユーザがシステムに対して自分自身を認証させる方法を提供しなければいけません。
どのように実現するかは、実装次第です。
実装は、認証のプロセスが初期化するためのページを提供しなければいけません。
`page`のソースを通してURIを認証プロセスに渡して、`"login"`を引数として渡します。

現在リクエストに結び付けられているユーザを`auth:current`コマンドで調べることができます。
ユーザが`"anonymous"`と解釈される場合は、`NIL`を返します。
詳しくはuserインターフェイスを参照ください。

`auth:*login-timeout*`, `auth:page`, `auth:current`, `auth:associate`もご覧ください。

### 2.3 ban
`banインターフェイス`を使うと、IP BANができます。
IP BANされたクライアントのIPアドレスからは、リクエストするページに対してアクセス出来なくなります。
BANは、タイムアウトの後、手動・自動いずれでも、離す(lift)することができます。
実装としては、ユーザのIPを監視するために追加で労力を割くするようなことは想定していません。

`ban:jail`, `ban:list`, `ban:jail-time`, `ban:release`をご参照ください。

### 2.4 キャッシュ
キャッシュインターフェイスは、一般的なキャッシュの仕組みを提供します。カスタマイズ可能で無効化のテストをもっています。
キャッシュを明示的に新しくするには、`cache:renew`をつかいます。
`cache:with-cache`を使うと、キャッシュの部品を構築することができ、テストのformがtrueと評価されたときに、BODYのキャッシュ値が返されるようにできます。

キャッシュ値が保存される方法は実装によります。 `cache:get`か`cache:with-cache`を使うと、キャッシュ値は文字列かバイト列に強制変換されます。
実装では、キャッシュに格納できる　typeには制限がありませんが、少なくとも、文字列とバイト列はサポートします。

キャッシュ値の名前は、名前とパッケージ名が、次の文字を含まないシンボルでなければいけません:`<>:"/\|?*.` 
キャッシュ値の変形は、`princ`によって表示される表現とは区別されるオブジェクトである必要があります。
その際には、先ほどと同じ文字の制限が適用されます。

`cache:get`, `cache:renew`, `cache:with-cache`をご参照ください。

### 2.5 データベース
このインターフェイスは、データを持続させるためのレイヤーを提供します。通常は、データベースと呼ばれるレイヤーです。
リレーショナル型のデータベースである必要はありませんが、そうであってもいいです。
実装の変数を保持するために、基本的なデータベースの機能しかサポートされません。(joinsやtriggers等はありません)
データ型も、整数、float、文字列に限定されます。
これらの制限にも関わらず、多くのアプリケーションにおいて、データベースインターフェイスはとても役に立ちます。

伝統的なRDMBの用語と区別するために、特別な用語が使われます:
`schema`は"structure"、`table`は"collection"、`row`は"record"とします。

データベースに接続する前に、データベース関連の命令を実行すると、未定義の動作を招きます。
Radianceでは、Radianceが動作している間はデータベースが接続されることを保障しているので、どのページ、どのAPI、どのURIディスパッチャーの定義において、データベースインターフェイスを問題なく使えます。

実際にデータ保存を行うための関数は、`db:insert`、`db:remove`、`db:update`、`db:select`、`db:iterate`です。
それらは、あなたが期待するように動作するはずです。

詳しくは、それぞれの関数に書かれているコメントを読んでください。
コレクションの作り方、どのような制限があるかを知りたい方は、`db:create`のコメントも参考にしてください。

データベースは、データ操作が完了すれば、Radianceを再起動したり、Lispイメージ、マシンがクラッシュしたとしても、、データの変更は存続されなければいけません。

`database:condition`, `database:connection-failed`, `database:connection-already-open`, `database:collection-condition`, `database:invalid-collection`, `database:collection-already-exists`, `database:invalid-field`, `database:id`, `database:ensure-id`, `database:connect`, `database:disconnect`, `database:connected-p`, `database:collections`, `database:collection-exists-p`, `database:create`, `database:structure`, `database:empty`, `database:drop`, `database:iterate`, `database:select`, `database:count`, `database:insert`, `database:remove`, `database:update`, `database:with-transaction`, `database:query`, `database:connected`, `database:disconnected`を参照してください。

### 2.6 logger

ログの関数を提供します。システムの中で関連して起きていることについて、ログのメッセージを出すことができます。
ログの出力内容と出力方法に関しては、実装とシステムの管理者次第です。

`logger:log`, `logger:trace`, `logger:debug`, `logger:info`, `logger:warn`, `logger:error`, `logger:severe`, `logger:fatal`をご覧ください。

### 2.7 メール
メールを送る仕組みを組み込むことができます。
様々なコンポーネントが、Webサイトの外からユーザとつながるために、メールのアクセスが必要になるかもしれません。
リモートサーバ、ローカル環境でのメール送信等、メールの送信方法の設定は、処理系依存です。
`mail:send`のフックを使うと、メールが送られる前に、メールに反応することができます。

`mail:send`をご覧ください。

### 2.8 プロフィール
プロフィールインターフェイスは、userにある種のpresenceをもたせたいアプリケーションにおいて、共通で使用されるuserインターフェイスを拡張できるようにします。
そのインターフェイスは、機能の一部として、ユーザの**プロフィール**が表示されるページを提供する必要があります。
その**プロフィール**は、数種類のパネルを表示しなければいけません。
パネルは、他のモジュールによって提供されており、`profile:define-panel`で追加できます。

`page`のリソースの型を通して、URIをプロファイルのページに移動することができます。

そのインターフェイスは、視覚的にユーザを特定させるために`profile:avatar`で**アバター画像**にアクセスさせることもできます。
また、`profile:name`を使うと、ユーザがユーザ名をカスタマイズできます。
さらに、`profile:fields`、`profile:add-field`、`profile:remove-field`を使うと、どのようなデータをユーザの属性に含むか、それを公(public)に表示させるかどうかを指定できます。

`profile:page`, `profile:avatar`, `profile:name`, `profile:fields`, `profile:add-field`, `profile:remove-field`, `profile:list-panels`, `profile:remove-panel`, `profile:define-panel`, `profile:panel`をご参照ください。

### 2.9 rate

[Rate limitation](https://en.wikipedia.org/wiki/Rate_limiting)の仕組みを提供します。秘密情報やコストが高いリソースへの負荷の高いアクセスを防ぐことができます。
２つの段階があります。第1段階は、`rate:define-limit`により、特定のリソースに対して、Rate limitationの動作を定義します。第1段階は、リソースが`rate:with-limitation`により保護されます。

もし、特定のユーザからのblockへのアクセスが頻繁すぎる場合は、blockは呼び出されません。制限の定義があるコードが、代わりに実行されます。

`Rate limitation`は、クライアント、ユーザ、セッションごとですが、グローバルではないことに注意してください。

`rate:define-limit`, `rate:left`, `rate:with-limitation`をご参照ください。

### 2.10 サーバ

serverインターフェイスとloggerインターフェイスは、唯一、Radianceが起動時に順番通りに読み込まれるものです。
HTTPリクエストを受け入れて、応答する責任があります。
実装では、リクエストを受け入れて、Radianceの`request`関数に渡す必要があります。
その後、`response`はリクエスト主に戻されます。

リスナーの動作を特定する引数は実装によることに注意してください。
しかし、実装は、`(mconfig :radiance :port)`で設定された`localhost`とポートからでアクセスできる標準のリスナーを提供して、`radiance:startup`で起動できるようにする必要があります。

`server:start`, `server:stop`, `server:listeners`, `server:started`, `server:stopped`をご参照ください。

### 2.11 セッション
あるクライアントが行う複数のリクエストを追跡します。
クライアントによっては情報を隠蔽したり偽装している場合があるので、完全にはクライアントを追跡できるとはいえません。
しかし、多くのユーザに対しては、うまく動作するはずです。
セッションインターフェイスは、他のインターフェイスや低レイヤーのライブラリの中で使われます。ユーザ認証にような一貫性を保つために使われます。

`session:*default-timeout*`, `session:session`, `session:=`, `session:start`, `session:get`, `session:list`, `session:id`, `session:field`, `session:timeout`, `session:end`, `session:active-p`, `session:create`をご参照ください。

### 2.12 ユーザ
ユーザオブジェクトを永続させ、パーミションの仕組みを組み込めます。
ユーザ認証、ユーザの特定、トラッキング等は扱いません。
このインターフェイスでは、ユーザオブジェクトを提供するのみであり、パーミション情報が管理されます。

パーミションに関する詳細は、`user:user`を参照してください。

`user:condition`, `user:not-found`, `user:user`, `user:=`, `user:list`, `user:get`, `user:username`, `user:fields`, `user:field`, `user:remove-field`, `user:remove`, `user:check`, `user:grant`, `user:revoke`, `user:add-default-permissions`, `user:create`, `user:remove`, `user:action`, `user:ready`, `user:unready`もご覧ください。

## 参考文献

* [modularize](https://shinmera.github.io/modularize) For the primary package metadata system
* [modularize-interfaces](https://shinmera.github.io/modularize-interfaces) For the interface and implementations extensions
* [modularize-hooks](https://shinmera.github.io/modularize-hooks) For the hooks and triggers mechanisms
* [ubiquitous](https://shinmera.github.io/ubiquitous) For configuration management
* [radiance-contribs](https://shirakumo.org/projects/radiance-contribs) Default interface implementations and other convenience modules for Radiance
