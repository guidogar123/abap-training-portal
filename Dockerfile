# Stage 1: Build
FROM node:20-alpine AS build

WORKDIR /app

# Instalar dependencias
COPY package*.json ./
RUN npm install

# Copiar código fuente y compilar
COPY . .
RUN npm run build

# Stage 2: Serve with Nginx
FROM nginx:stable-alpine

# Copiar archivos estáticos del build
COPY --from=build /app/dist /usr/share/nginx/html

# Copiar configuración de Nginx para SPA
COPY nginx.conf /etc/nginx/conf.d/default.conf

# Redirigir logs de nginx a stdout/stderr (estándar en imagen oficial pero aseguramos)
RUN ln -sf /dev/stdout /var/log/nginx/access.log && ln -sf /dev/stderr /var/log/nginx/error.log

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
