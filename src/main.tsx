import React from 'react'
import ReactDOM from 'react-dom/client'
import { PublicClientApplication } from "@azure/msal-browser";
import { MsalProvider } from "@azure/msal-react";
import { msalConfig } from "./lib/msalConfig";
import App from './App.tsx'
import './index.css'

const msalInstance = new PublicClientApplication(msalConfig);

// Initialize MSAL before rendering
msalInstance.initialize().then(() => {
    ReactDOM.createRoot(document.getElementById('root')!).render(
        <React.StrictMode>
            <MsalProvider instance={msalInstance}>
                <App />
            </MsalProvider>
        </React.StrictMode>,
    );
});
