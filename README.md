# Portal de Capacitación SAP ABAP

Aplicación web moderna para el curso de "Master en SAP ABAP". Construida con React + Vite.

## Características
- **Temario Completo**: 4 Semanas de contenido, desde DDIC hasta Ampliaciones avanzadas.
- **Diseño Premium**: Dark Mode, Glassmorphism y tipografía cuidada.
- **Interactividad**: Bloques de código con resaltado de sintaxis ABAP y botón de copia.
- **Responsive**: Funciona en PC y Tablet.

## Instalación

1.  Abre una terminal en esta carpeta.
2.  Instala las dependencias:
    ```bash
    npm install
    # Si faltan las librerías de UI:
    npm install lucide-react react-syntax-highlighter react-router-dom framer-motion
    ```
3.  Inicia el servidor de desarrollo:
    ```bash
    npm run dev
    ```

## Despliegue en GitHub Pages

1.  Ajusta `vite.config.js` agregando la propiedad `base` con el nombre de tu repositorio:
    ```js
    export default defineConfig({
      plugins: [react()],
      base: '/nombre-de-tu-repo/', // <--- IMPORTANTE
    })
    ```
2.  Ejecuta el build:
    ```bash
    npm run build
    ```
3.  Sube el contenido de la carpeta `dist` a la rama `gh-pages` de tu repositorio, o configura GitHub Pages para leer de la carpeta `root` si subes solo el dist (no recomendado), lo mejor es usar una GitHub Action o subir todo el código y que la action haga el build.
