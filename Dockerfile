# --- Build Stage ---
FROM node:20-alpine AS build
WORKDIR /app

# Argumentos con valores por defecto (Garantizan que el build no sea nulo)
ARG VITE_MICROSOFT_CLIENT_ID=3405f9b5-f33d-4153-8897-f01bcc592ad1
ARG VITE_MICROSOFT_TENANT_ID=08952c25-7320-4ffb-8f36-899990c950ad

ENV VITE_MICROSOFT_CLIENT_ID=$VITE_MICROSOFT_CLIENT_ID
ENV VITE_MICROSOFT_TENANT_ID=$VITE_MICROSOFT_TENANT_ID

# Verificar variables en logs de build
RUN echo "Building with Client ID: ${VITE_MICROSOFT_CLIENT_ID}"

COPY package*.json ./
RUN npm install

COPY . .
RUN npm run build

# Verificar que el build generó archivos
RUN ls -la /app/dist

# --- Production Stage ---
FROM nginx:stable-alpine

# Instalar curl para salud
RUN apk add --no-cache curl

# Configurar logs de Nginx
RUN ln -sf /dev/stdout /var/log/nginx/access.log && ln -sf /dev/stderr /var/log/nginx/error.log

COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
