# --- Build Stage ---
FROM node:20-alpine AS build
WORKDIR /app

# Argumentos con valores por defecto (tomados del .env actual)
ARG VITE_MICROSOFT_CLIENT_ID=3405f9b5-f33d-4153-8897-f01bcc592ad1
ARG VITE_MICROSOFT_TENANT_ID=08952c25-7320-4ffb-8f36-899990c950ad

ENV VITE_MICROSOFT_CLIENT_ID=$VITE_MICROSOFT_CLIENT_ID
ENV VITE_MICROSOFT_TENANT_ID=$VITE_MICROSOFT_TENANT_ID

# Instalación estándar
COPY package*.json ./
RUN npm install

# Build
COPY . .
RUN npm run build

# --- Production Stage ---
FROM nginx:stable-alpine
COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
