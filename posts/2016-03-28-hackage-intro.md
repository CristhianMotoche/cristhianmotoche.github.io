---
title: Building - Subir un paquete a Hackage - ES
date: 2016-03-21 20:57:22
tags: hackage, haskell
metadescription: En este blog explicaré cómo puedes subir un paquete de un proyecto de Haskell a Hackage.
---

## ¿Qué es Hackage?

[Hackage][hackage] es la central de paquetes de la comunidad
de Haskell para projectos de código abierto. En el [sitio Web oficial][hackage]
puedes encontrar tanto librerías como programas desarrollados por la comunidad.

## Primer paso - Crear una cuenta en Hackage
Para crear una cuenta en Hackage, ve al [sitio de registro][hackage-register]
e ingresa el nombre con el cual te conocerán en Hackage, tu nombre de usuario
para ingresar al sistema y tu dirección de correo electrónico. Y da click en el
botón para solicitar una cuenta (Request Account).

![**Imagen 1. Registro**][1]

Recibirás un correo para la verificación de la cuenta:

![**Imagen 2. Verificación de correo**][2]

Solo debes dar clic en el enlace y te llevará a la página para que establezcas tu
contraseña:

![**Imagen 3. Establecer contraseña**][3]

Luego de establecer tu contraseña y dar clic en crear cuenta (Create Account). Serás
rediridigo a la página de bienvenida:

![**Imagen 4. Establecer contraseña**][4]

## Segundo paso - Crear tu paquete

Para la creación de mi paquete voy a utilizar `stack`; también puedes utilizar otras
herramientas, por ejemplo [hi](https://github.com/fujimura/hi).
Y también puedes utilizar `cabal` si lo deseas;
de hecho, utilizaré `cabal` para comprobar que la estructura e información de mi
proyecto esté correcta.

Primero, tienes que crear el nuevo paquete:

```shell
$ stack new [package-name]
```

Y editamos el

Y verificamos si tiene la información necesaria y si está correctamente estructurado
utilizando `cabal`.

```shell
$ cabal check
No errors or warnings could be found in the package.
```
**NOTA:** Si el comando anterior falla, no podrás subir tu paquete a Hackage, será
automáticamente descartado.

## Tercer paso - Subir tu paquete

Para finalmente subir tu paquete a Hackage, tienes que crear una distribución del
código fuente (source distribution).

```shell
$ stack sdist
Getting file list for /home/user/repos/[package-name]/
Building sdist tarball for /home/user/repos/[package-name]/
Checking package '[package-name]' for common mistakes
Wrote sdist tarball to /home/user/repos/[package-name]/.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/[package-name]-0.1.0.0.tar.gz
```

El comando anterior generará un archivo comprimido en `.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/[package-name]-0.1.0.0.tar.gz`.
Este será el archivo que deberás subir a Hackage, en el [este enlace][hackage-upload].

Espero que les haya gustado, cualquier comentario o sugerencia será bienvenido.
ByE!

[hackage]: http://hackage.haskell.org/
[hackage-register]: http://hackage.haskell.org/users/register-request
[hackage-upload]: https://hackage.haskell.org/packages/candidates/upload

[1]: /images/hackage-intro-img-register_640x480_resized.png {width=600px}
[2]: /images/hackage-intro-email-verificacion.png {width=400px}
[3]: /images/hackage-intro-password_640x480_resized.png {width=400px}
[4]: /images/hackage-intro-welcome-message_640x480_resized.png {width=400px}
