# Stage 1: Build
FROM node:20-alpine AS build

WORKDIR /app

# Argumentos de construcción para Vite
ARG VITE_MICROSOFT_CLIENT_ID
ARG VITE_MICROSOFT_TENANT_ID

# Exponerlos como variables de entorno para el proceso de build
ENV VITE_MICROSOFT_CLIENT_ID=$VITE_MICROSOFT_CLIENT_ID
ENV VITE_MICROSOFT_TENANT_ID=$VITE_MICROSOFT_TENANT_ID

# Instalar dependencias limpias
COPY package*.json ./
RUN npm ci

# Copiar código fuente y compilar
COPY . .
RUN npm run build

# Stage 2: Serve
FROM nginx:stable-alpine

# Copiar archivos estáticos
COPY --from=build /app/dist /usr/share/nginx/html

# Copiar configuración específica
COPY nginx.conf /etc/nginx/conf.d/default.conf

# Validar sintaxis
RUN nginx -t

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
