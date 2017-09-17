---
title: Subir un paquete a Hackage
date: 2016-05-31 18:34:55
tags: hackage, haskell, es
description: En este blog explicaré cómo puedes subir un paquete de un proyecto de Haskell a Hackage.
---

## ¿Qué es Hackage?

[Hackage][hackage] es la central de paquetes de la comunidad
de Haskell para projectos de código abierto. En el [sitio Web oficial][hackage]
puedes encontrar tanto librerías como programas desarrollados por la comunidad de
Haskell.

## Primer paso - Crear una cuenta en Hackage
Primero ve al [sitio de registro][hackage-register]
e ingresa el nombre con el cual te conocerán en Hackage, tu nombre de usuario
para ingresar al sistema y tu dirección de correo electrónico. Da clic en el
botón para solicitar una cuenta (Request Account).

![**Imagen 1. Registro**][1]

Recibirás un correo para la verificación de la creación de la cuenta:

![**Imagen 2. Verificación de correo**][2]

Debes dar clic en el enlace que te llevará Hackage, para que establezcas tu
contraseña:

![**Imagen 3. Establecer contraseña**][3]

Luego de establecer tu contraseña y dar clic en crear cuenta (Create Account). Serás
rediridigo a la página de bienvenida:

![**Imagen 4. Establecer contraseña**][4]

## Segundo paso - Crear tu paquete

Obviamente, el motivo para crear una cuenta es subir un paquete a Hackage.
Para la creación de un paquete puedes utilizar `stack` - yo utilizaré esta herramienta -;
o también otras herramientas, por ejemplo [hi](https://github.com/fujimura/hi).
Por supuesto, también puedes utilizar `cabal`;
de hecho, utilizaré `cabal` para comprobar que la estructura e información de mi
proyecto esté correcta.

Primero, tienes que crear el nuevo paquete:

```shell
$ stack new [package-name]
```

Y editamos el archivo `[package-name].cabal`, colocando toda la información que
corresponda, por ejemplo, la dirección del código fuente, la licencia, el autor
del paquete, etc.

Verificamos si tiene la información necesaria y si está correctamente
estructurado utilizando `cabal`.

```shell
$ cabal check
No errors or warnings could be found in the package.
```
**NOTA:** Si el comando anterior falla, no podrás subir tu paquete a Hackage,
ya que será automáticamente descartado.

## Tercer paso - Subir tu paquete

Finalmente, para subir tu paquete a Hackage, tienes que crear una distribución del
código fuente (source distribution). Yo utilizaré `stack`, pero también puedes hacerlo
con `cabal`.

```shell
$ stack sdist
Getting file list for /home/user/repos/[package-name]/
Building sdist tarball for /home/user/repos/[package-name]/
Checking package '[package-name]' for common mistakes
Wrote sdist tarball to /home/user/repos/[package-name]/.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/[package-name]-0.1.0.0.tar.gz
```

El comando anterior generará un archivo comprimido:
`.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/[package-name]-0.1.0.0.tar.gz`.
El cual deberás subir a Hackage, yendo al siguiente [enlace][hackage-upload].
Cuando intentes subir el paquete te pedirá que ingreses tus credenciales con las
cuales hayas creado tu cuenta en Hackage. Las ingresas y ¡está listo! Podras ver
tu paquete en la lista de [paquetes candidatos][hackage-candidate-packages].

Espero que te haya gustado, cualquier comentario, sugerencia o preguta la puedes
hacer en los comentarios de Disqus. Te aseguro que contestaré tan rápido como pueda. `>:-D`
ByE!

[hackage]: http://hackage.haskell.org/
[hackage-register]: http://hackage.haskell.org/users/register-request
[hackage-upload]: https://hackage.haskell.org/packages/candidates/upload
[hackage-candidate-packages]: https://hackage.haskell.org/packages/candidates/

[1]: /images/hackage-intro-img-register_640x480_resized.png
[2]: /images/hackage-intro-email-verificacion.png
[3]: /images/hackage-intro-password_640x480_resized.png
[4]: /images/hackage-intro-welcome-message_640x480_resized.png
