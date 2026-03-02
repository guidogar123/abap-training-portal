import React, { createContext, useContext, useEffect, useState } from 'react';
import { useMsal, useIsAuthenticated } from '@azure/msal-react';
import { loginRequest } from '../lib/msalConfig';

interface AuthContextType {
    user: any | null;
    isAuthenticated: boolean;
    loading: boolean;
    login: () => Promise<void>;
    logout: () => Promise<void>;
}

const AuthContext = createContext<AuthContextType>({
    user: null,
    isAuthenticated: false,
    loading: true,
    login: async () => { },
    logout: async () => { },
});

export const useAuth = () => useContext(AuthContext);

export const AuthProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { instance, accounts } = useMsal();
    const isMsAuthenticated = useIsAuthenticated();
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        const checkAuth = async () => {
            try {
                await instance.handleRedirectPromise();
                setLoading(false);
            } catch (error) {
                console.error("Redirect check error:", error);
                setLoading(false);
            }
        };
        checkAuth();
    }, [instance]);

    const user = accounts.length > 0 ? accounts[0] : null;

    const login = async () => {
        try {
            await instance.loginRedirect({
                ...loginRequest,
                redirectUri: window.location.origin,
                extraQueryParameters: {
                    domain_hint: 'organizations'
                }
            });
        } catch (error) {
            console.error("Login error:", error);
        }
    };

    const logout = async () => {
        try {
            await instance.logoutRedirect({
                postLogoutRedirectUri: "/",
            });
        } catch (error) {
            console.error("Logout error:", error);
        }
    };

    return (
        <AuthContext.Provider value={{
            user,
            isAuthenticated: isMsAuthenticated,
            loading,
            login,
            logout
        }}>
            {children}
        </AuthContext.Provider>
    );
};
